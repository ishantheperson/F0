module Parser.Imports where 

import LibraryBindings 

import Parser.AST
import Parser.Internal 

import Text.Megaparsec 
import Text.Megaparsec.Char

data ImportLocation = 
    C0Library String 
  | C0SourceFile FilePath 

f0Import :: Parser (ImportLocation, [(String, F0Type)])
f0Import = (,) <$> (reserved "import" *> f0ImportLoc) 
               <*> (parens $ sepBy1 f0ImportedBinding (symbol ","))

f0ImportLoc :: Parser ImportLocation
f0ImportLoc = 
  (C0Library <$> between (symbol "<") (symbol ">") identifier) <|>
  (C0SourceFile <$> between (symbol "\"") (symbol "\"") identifier)

f0ImportedBinding :: Parser (String, F0Type)
f0ImportedBinding = (,) <$> identifier <*> (symbol ":" *> f0Type) 
