{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
module Parser.AST where 

import Text.Megaparsec.Pos 

type SourceRange = (SourcePos, SourcePos)

-- | Type of a declaration where identifiers
-- are represented by type "symbol". This lets
-- us later write a transformation to turn
-- the symbols which are strings into
-- a better symbol type which guarantees no shadowing
data F0Declaration symbol typeInfo =  
    F0Value symbol (typeInfo F0Type) (F0Expression symbol typeInfo) -- ^ val Name (: Type) = Expression
  | F0Fun symbol [(symbol, typeInfo F0Type)] (typeInfo F0Type) (F0Expression symbol typeInfo) -- ^ fun Name
  | F0DeclPos SourcePos (F0Declaration symbol typeInfo) SourcePos 

data F0Expression symbol typeInfo = 
    -- F0Fix symbol (typeInfo F0Type) (F0Expression symbol typeInfo) -- ^ fix Name as Expression 
    F0Lambda symbol (typeInfo F0Type) (F0Expression symbol typeInfo) -- ^ fn x (: t) => etc 
  | F0App (F0Expression symbol typeInfo) (F0Expression symbol typeInfo) -- ^ e1 e2 
  | F0If (F0Expression symbol typeInfo) (F0Expression symbol typeInfo) (F0Expression symbol typeInfo) -- ^ if e1 then e2 else e3 
  | F0IntLiteral Integer  
  | F0StringLiteral String 
  | F0Identifier symbol  
  | F0TypeAssertion (F0Expression symbol typeInfo) F0Type
  | F0OpExp F0Operator [F0Expression symbol typeInfo] -- ^ arithmetic ops, comparison ops, etc. 
  | F0ExpPos SourcePos (F0Expression symbol typeInfo) SourcePos -- ^ Start, Expression, End 

deriving instance ((Show (typeInfo F0Type), Show symbol) => Show (F0Declaration symbol typeInfo))
deriving instance ((Show (typeInfo F0Type), Show symbol) => Show (F0Expression symbol typeInfo))

deriving instance ((Eq (typeInfo F0Type), Eq symbol) => Eq (F0Declaration symbol typeInfo))
deriving instance ((Eq (typeInfo F0Type), Eq symbol) => Eq (F0Expression symbol typeInfo))

data F0Operator = 
    Plus 
  | Times 
  | Equals
  deriving (Show, Eq)  

data F0Type = 
    F0Int 
  | F0String 
  | F0Bool
  | F0Alias 
  | F0Function F0Type F0Type 
  deriving (Show, Eq)