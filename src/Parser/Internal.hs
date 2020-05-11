{-# LANGUAGE LambdaCase #-}
module Parser.Internal where 

import Data.Void 
import Data.Maybe (catMaybes)
import Control.Monad (void)

import Parser.AST 

import Text.Megaparsec 
import Text.Megaparsec.Char
import Text.Megaparsec.Debug 
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad.Combinators.Expr

import Debug.Trace 

type Parser = Parsec Void String 
-- type Parser = ParsecT Void String (Writer [ParseError String Void])

f0Decls :: Parser [F0Declaration String Maybe]
f0Decls = concat <$> many f0Decl
  where recover = id 
          -- Recovery is right now broken since it messes up
          -- let declarations 
          -- (the failure to find another decl causes it to skip ahead)
          -- Could probably be fixed by checking if the error token is "in"
          {-withRecovery $ \e -> do
          registerParseError e
          -- Advance to next "fun" or "val"
          someTill anySingle (void . lookAhead $ choice (reserved <$> ["fun", "val"]) <|> eof)
          return Nothing -}

data Pattern = Name String | Tuple [String] | Discard

-- | May generate multiple bindings as a result of a tuple binding
f0Decl :: Parser [F0Declaration String Maybe]
f0Decl = positioned (fun <|> val <|> datatype)
    where val = do 
            reserved "val"
            name <- pat 
            symbol "="
            e <- f0Expression
            case name of 
              Name s -> return [F0Value s Nothing e]
              Discard -> return [F0Value "_discard" Nothing e]
              Tuple ts -> return ([F0Value "_tuple" Nothing e] ++ go ts 0)
                where numItems = length ts 
                      go [] _ = [] 
                      go (t:ts) i = (F0Value t Nothing (F0TupleAccess i numItems (F0Identifier "_tuple"))) : go ts (i + 1)

          fun = do 
            name <- reserved "fun" *> identifier  
            args <- zip [0..] <$> some pat 
            e <- symbol "=" *> f0Expression
            let desugarArg :: (Int, Pattern) -> F0Expression String Maybe -> F0Expression String Maybe
                desugarArg (i, p) e = case p of 
                  Name s -> e 
                  Discard -> e 
                  Tuple ts -> desugarTuple ts ("_tuple" ++ show i) e 

                newFunctionBody = foldr desugarArg e args

                transformArg :: (Int, Pattern) -> String 
                transformArg = \case 
                  (_, Name s) -> s 
                  (_, Discard) -> "_discard"
                  (i, Tuple _) -> "_tuple" ++ show i 

                -- TODO: use arrows here lmao 
                functionArgs = map (\a -> (transformArg a, Nothing)) args  

            return [F0Fun name functionArgs Nothing newFunctionBody]

          datatype = do 
            reserved "datatype"
            tvs <- (pure <$> typeVariable) <|> parens (sepBy typeVariable (symbol ",")) <|> pure []
            name <- identifier 
            symbol "="

            optional (symbol "|") -- Allow leading bar 
            dataCases <- sepBy1 dataCase (symbol "|")

            return [F0Data tvs name dataCases] 

            where dataCase :: Parser (String, F0Type)
                  dataCase = (,) <$> identifier <*> ((reserved "of" *> f0Type) <|> pure f0UnitT)

          positioned p = p
            -- F0DeclPos <$> getSourcePos <*> p <*> getSourcePos

f0Expression :: Parser (F0Expression String Maybe)
f0Expression = makeExprParser (term >>= postfix) operators <?> "expression"
  where f0Lambda = do 
          name <- reserved "fn" *> pat 
          e <- symbol "=>" *> f0Expression
          return $ case name of 
                     Name n -> F0Lambda n Nothing e 
                     Discard -> F0Lambda "_discard" Nothing e 
                     Tuple ts -> F0Lambda "_tuple" Nothing (desugarTuple ts "_tuple" e)

        f0If = F0If <$> (reserved "if" *> f0Expression) 
                    <*> (reserved "then" *> f0Expression) 
                    <*> (reserved "else" *> f0Expression)
        f0IntLiteral = F0Literal . F0IntLiteral <$> integer 
        f0StringLiteral = F0Literal . F0StringLiteral <$> stringLiteral
        f0BoolLiteral = F0Literal . F0BoolLiteral <$> ((True <$ reserved "true") <|> (False <$ reserved "false")) 
        f0UnitLiteral = F0Literal F0UnitLiteral <$ symbol "()"

        f0Ident = F0Identifier <$> identifier
        f0Let = do 
          reserved "let"
          decls <- f0Decls 
          reserved "in"
          e <- f0Expression 
          reserved "end"
          return $ foldr F0Let e decls 

        f0Tuple = do 
          symbol "("
          elems <- sepBy1 f0Expression (symbol ",")
          symbol ")"
          return $ case elems of 
                     [x] -> x -- Don't allow tuple of one element
                     _ -> F0Tuple elems 

        f0Case = do 
          obj <- reserved "case" *> f0Expression
          reserved "of"
          optional $ symbol "|"
          rules <- sepBy1 caseRule (symbol "|")
          return $ F0Case obj rules 

          where caseRule :: Parser (String, (String, F0Expression String Maybe))
                caseRule = do 
                  constructor <- identifier 
                  x <- option "_unused" $ identifier <|> ("_unused" <$ (symbol "_" <|> symbol "()"))
                  symbol "=>"
                  e <- f0Expression
                  return (constructor, (x, e))

        term = positioned $ 
          choice [f0Let, f0Lambda, f0If, f0Case,
                  f0UnitLiteral, f0IntLiteral, f0StringLiteral, f0BoolLiteral, 
                  f0Ident, f0Tuple]
        postfix e = 
              positioned (functionApp e) <|> return e 
          -- <|> positioned (F0TypeAssertion e <$> (symbol ":" >> f0Type))

        functionApp e = foldl F0App e <$> some term 

        operators = [[prefixOp "!" Not],
                     [binOp Times,
                      binOp Divide],
                     [binOp Plus,
                      binOp Minus],
                     [binOp LessThan],
                     [binOp Equals],
                     [binOp And],
                     [binOp Or],
                     [semicolon]]
          where prefixOp opString opConstructor = 
                  Prefix (symbol opString *> return (\a -> F0OpExp opConstructor [a]))
                binOp opConstructor = 
                  InfixL (symbol (printOp opConstructor) *> return (\a b -> F0OpExp opConstructor [a, b])) 

                semicolon = 
                  InfixR (symbol ";" *> return (\a b -> F0Let (F0Value "_discard" Nothing a) b))
        positioned p =   -- Source information for expressions can clutter up the AST a lot
                         -- so right now I am removing it 
            F0ExpPos <$> getSourcePos <*> p <*> getSourcePos

-- | Takes a tuple pattern and creates bindings for it in a subexpression e 
desugarTuple :: [String] -> String -> F0Expression String Maybe -> F0Expression String Maybe 
desugarTuple tupleNames tupleName e = go tupleNames 0 e 
  where numItems = length tupleNames 
        go [] _ e = e 
        go ("_":ts) i e = go ts (i + 1) e 
        go ("()":ts) i e = go ts (i + 1)e 
        go (t:ts) i e = go ts (i + 1) (F0Let (F0Value t Nothing (F0TupleAccess i numItems (F0Identifier tupleName))) e)

pat :: Parser Pattern 
pat = 
      (Name <$> identifier) 
  <|> (Discard <$ (symbol "()" <|> symbol "_")) 
  <|> (parens $ Tuple <$> (sepBy1 (identifier <|> string "_") (symbol ",")))
  <?> "pattern"

-- Really only used when parsing data types
f0Type :: Parser F0Type
f0Type = makeExprParser (term >>= postfixA) operators <?> "type"
  where term =  
              F0PrimitiveType F0IntType <$ reserved "int"
          <|> F0PrimitiveType F0StringType <$ reserved "string" 
          <|> F0PrimitiveType F0BoolType <$ reserved "bool"
          <|> F0PrimitiveType F0UnitType <$ reserved "unit"
          <|> F0TypeVariable <$> typeVariable 
          -- <|> F0TypeIdent <$> identifier
          <|> typeVarTuple 
          <|> parens f0Type 
        
        -- A little hacky, but whats happening is that
        -- we need to know if we are parsing a tuple
        -- type or not. In postfixA we aren't necessarily,
        -- but in postfixB we are.
        postfixA e = tupleType e <|> typeApp e postfixA <|> return e
        postfixB e = typeApp e postfixB <|> return e
        tupleType e = do 
          ts <- some (symbol "*" *> (term >>= postfixB))  
          return $ F0TupleType (e:ts)

        typeApp e next = 
             (identifier >>= \name -> next (F0TypeCons e name))
          --foldl F0TypeCons e <$> some term 

        operators = [[InfixR (F0Function <$ symbol "->")]]

typeVarTuple :: Parser F0Type
typeVarTuple = try $ parens $ do  
  tvs <- sepBy f0Type (symbol ",")
  return $ case tvs of 
             [t] -> t
             _ -> F0TypeTuple tvs 

typeVariable :: Parser String
typeVariable = char '\'' *> identifier <?> "type variable"

-- | Parses a name and maybe a type assertion with it
name :: Parser (String, Maybe F0Type)
name = unitName <|> ((,) <$> identifier <*> typeAnnotation <|> parens name)
  where unitName = ("_unit", Just $ F0PrimitiveType F0UnitType) <$ symbol "()"

typeAnnotation :: Parser (Maybe F0Type)
typeAnnotation = optional (symbol ":" *> f0Type) <?> "type annotation"

-- Lexing 
symbol :: String -> Parser String
symbol = Lex.symbol sc

stringLiteral :: Parser String
stringLiteral = lexeme (char '"' >> manyTill Lex.charLiteral (char '"')) <?> "string" 
charLiteral :: Parser Char 
charLiteral = lexeme (char '\'' >> (Lex.charLiteral <* char '\'')) <?> "character"

-- | Parses an integer (the integer is not checked for being in bounds)
integer :: Parser Integer 
integer = lexeme (try $ char '0' >> char' 'x' >> Lex.hexadecimal) <|> lexeme Lex.decimal

identifier :: Parser String
identifier = (lexeme . try) (p >>= check) <?> "identifier"
  where p = (:) <$> identStart <*> many identLetter
        identStart = letterChar
        identLetter = alphaNumChar <|> char '_' <|> char '\''

        check x = if x `elem` reservedWords
                    then fail $ "'" ++ x ++ "' cannot be an identifier"
                    else return x

reserved :: String -> Parser ()
reserved word = (lexeme . try) (string word *> notFollowedBy alphaNumChar)

reservedWords :: [String]
reservedWords = ["val",
                 "fun",
                 "if",
                 "then",
                 "else",
                 "fn",
                 "int",
                 "string",
                 "bool",
                 "unit",
                 "let",
                 "in",
                 "end",
                 "case",
                 "of",
                 "datatype"]

parens, lexeme :: Parser a -> Parser a 
parens = between (symbol "(") (symbol ")")
lexeme = Lex.lexeme sc 

sc, lineComment, blockComment :: Parser () 
sc = Lex.space space1 lineComment blockComment
lineComment = do 
  void $ string "--"
  void $ manyTill anySingle (char '\n')

blockComment = do 
  void $ string "(*"
  void $ manyTill anySingle (string "*)")
