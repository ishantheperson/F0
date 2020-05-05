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

type Parser = Parsec Void String 
-- type Parser = ParsecT Void String (Writer [ParseError String Void])

f0Decls :: Parser [F0Declaration String Maybe]
f0Decls = catMaybes <$> many (recover (Just <$> f0Decl)) 
  where recover = withRecovery $ \e -> do
          registerParseError e
          -- Advance to next "fun" or "val"
          someTill anySingle (void . lookAhead $ choice (reserved <$> ["fun", "val"]))
          return Nothing 

f0Decl :: Parser (F0Declaration String Maybe)
f0Decl = positioned fun <|> positioned val
    where val = uncurry F0Value <$> (reserved "val" *> name) <*> (symbol "=" *> f0Expression)
          fun = F0Fun <$> (reserved "fun" *> identifier) 
                      <*> some arg 
                      <*> optional (symbol ":" >> f0Type) 
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
        f0IntLiteral = F0IntLiteral <$> integer 
        f0StringLiteral = F0StringLiteral <$> stringLiteral
        f0Ident = F0Identifier <$> identifier
        
        term = 
          choice (parens f0Expression : (positioned <$> [f0Lambda, f0If, f0IntLiteral, f0StringLiteral, f0Ident]))
        postfix e = 
              positioned (F0App e <$> f0Expression)
          <|> positioned (F0TypeAssertion e <$> (symbol ":" >> f0Type))
          <|> return e 
        operators = [[binOp "*" Times],
                     [binOp "+" Plus],
                     [binOp "==" Equals]]
          where binOp opString opConstructor = 
                  InfixL (symbol opString >> return (\a b -> F0OpExp opConstructor [a, b])) 
        positioned p =   -- Source information for expressions can clutter up the AST a lot
                         -- so right now I am removing it 
          F0ExpPos <$> getSourcePos <*> p <*> getSourcePos

f0Type :: Parser F0Type
f0Type = makeExprParser term operators <?> "type"
  where term =  
              F0Int <$ reserved "int"
          <|> F0String <$ reserved "string" 
          <|> parens f0Type 
        
        operators = [[InfixR (F0Function <$ symbol "->") ]]

-- | Parses a name and maybe a type assertion with it
name :: Parser (String, Maybe F0Type)
name = ((,) <$> identifier <*> optional (symbol ":" >> f0Type)) <|> parens name 

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

reservedWords = ["val",
                 "fun",
                 "if",
                 "then",
                 "else",
                 "fn",
                 "int",
                 "string",
                 "fix"]

parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a 
lexeme = Lex.lexeme sc 

sc, lineComment, blockComment :: Parser () 
sc = Lex.space space1 lineComment blockComment
lineComment = do 
  void $ string "--"
  void $ manyTill anySingle (char '\n')

blockComment = do 
  void $ string "(*"
  void $ manyTill anySingle (string "*)")

