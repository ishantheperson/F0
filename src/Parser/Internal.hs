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
f0Decls = catMaybes <$> many (recover (Just <$> f0Decl)) 
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

f0Decl :: Parser (F0Declaration String Maybe)
f0Decl = positioned fun <|> positioned val
    where val = uncurry F0Value <$> (try $ reserved "val" *> name) <*> (symbol "=" *> f0Expression)
          fun = F0Fun <$> (try $ reserved "fun" *> identifier) 
                      <*> some arg 
                      <*> typeAnnotation
                      <*> (symbol "=" *> f0Expression)

          arg = parens arg <|> name <?> "function argument"

          positioned p = p
            -- F0DeclPos <$> getSourcePos <*> p <*> getSourcePos

f0Expression :: Parser (F0Expression String Maybe)
f0Expression = makeExprParser (term >>= postfix) operators 
  where f0Lambda = uncurry F0Lambda <$> (reserved "fn" *> name) 
                                    <*> (symbol "=>" *> f0Expression)
        f0If = F0If <$> (reserved "if" *> f0Expression) 
                    <*> (reserved "then" *> f0Expression) 
                    <*> (reserved "else" *> f0Expression)
        f0IntLiteral = F0Literal . F0IntLiteral <$> integer 
        f0StringLiteral = F0Literal . F0StringLiteral <$> stringLiteral
        f0BoolLiteral = F0Literal . F0BoolLiteral <$> boolLiteral 
        f0UnitLiteral = F0Literal F0UnitLiteral <$ symbol "()"

        f0Ident = F0Identifier <$> identifier
        f0Let = do 
          reserved "let"
          decls <- f0Decls 
          reserved "in"
          e <- f0Expression 
          reserved "end"
          return $ foldr F0Let e decls 

        boolLiteral = (True <$ reserved "true") <|> (False <$ reserved "false")

        term = positioned $ 
          choice [f0Let, f0Lambda, f0If, f0UnitLiteral, f0IntLiteral, f0StringLiteral, f0BoolLiteral, f0Ident, parens f0Expression]
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

f0Type :: Parser F0Type
f0Type = makeExprParser term operators <?> "type"
  where term =  
              F0PrimitiveType F0IntType <$ reserved "int"
          <|> F0PrimitiveType F0StringType <$ reserved "string" 
          <|> F0PrimitiveType F0BoolType <$ reserved "bool"
          <|> F0PrimitiveType F0UnitType <$ reserved "unit"
          <|> F0TypeVariable <$> typeVariable 
          -- <|> F0TypeIdent <$> identifier 
          <|> parens f0Type 
        
        operators = [[InfixR (F0Function <$ symbol "->") ]]
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

stringLiteral = lexeme (char '"' >> manyTill Lex.charLiteral (char '"')) <?> "string" 
charLiteral = lexeme (char '\'' >> (Lex.charLiteral <* char '\'')) <?> "character"

-- | Parses an integer (the integer is not checked for being in bounds)
integer :: Parser Integer 
integer = lexeme (try $ char '0' >> char' 'x' >> Lex.hexadecimal) <|> lexeme Lex.decimal

identifier = (lexeme . try) (p >>= check) <?> "identifier"
  where p = (:) <$> identStart <*> many identLetter
        identStart = letterChar
        identLetter = alphaNumChar <|> char '_' <|> char '\''

        check x = if x `elem` reservedWords
                    then fail $ "'" ++ x ++ "' cannot be an identifier"
                    else return x

-- reserved :: String -> Parser () 
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
                 "end"]

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
