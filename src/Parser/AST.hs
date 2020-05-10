{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
module Parser.AST where 

import Text.Megaparsec.Pos 

type SourceRange = (SourcePos, SourcePos)

printSourceRange :: Maybe SourceRange -> String 
printSourceRange Nothing = "<no position information available>"
printSourceRange (Just (start, end)) = sourcePosPretty start ++ " - " ++ sourcePosPretty end 

-- | Type of a declaration where identifiers
-- are represented by type "symbol". This lets
-- us later write a transformation to turn
-- the symbols which are strings into
-- a better symbol type which guarantees no shadowing
data F0Declaration symbol typeInfo =  
    F0Value symbol (typeInfo F0Type) (F0Expression symbol typeInfo) -- ^ val Name (: Type) = Expression
  | F0Fun symbol [(symbol, typeInfo F0Type)] (typeInfo F0Type) (F0Expression symbol typeInfo) -- ^ fun Name
  | F0Data [TypeVariable] String [(symbol, F0Type)] -- ^ datatype ('a, 'b, ..) foo = A of t1 | B of t2 ..
  | F0DeclPos SourcePos (F0Declaration symbol typeInfo) SourcePos 

data F0Expression symbol typeInfo = 
    -- F0Fix symbol (typeInfo F0Type) (F0Expression symbol typeInfo) -- ^ fix Name as Expression 
    F0Lambda symbol (typeInfo F0Type) (F0Expression symbol typeInfo) -- ^ fn x (: t) => etc 
  | F0App (F0Expression symbol typeInfo) (F0Expression symbol typeInfo) -- ^ e1 e2 
  | F0Let (F0Declaration symbol typeInfo) (F0Expression symbol typeInfo) -- ^ let decl in e end. Multiple decls become nested lets
  | F0If (F0Expression symbol typeInfo) (F0Expression symbol typeInfo) (F0Expression symbol typeInfo) -- ^ if e1 then e2 else e3 
  | F0Literal F0Literal 
  | F0TagValue String Int (F0Expression symbol typeInfo) -- ^ type 
  | F0Identifier symbol 
  | F0Tuple [F0Expression symbol typeInfo] -- ^ Construct a tuple
  | F0TupleAccess Int Int (F0Expression symbol typeInfo) -- ^ Access element i out of n in e 
  | F0TypeAssertion (F0Expression symbol typeInfo) F0Type -- Removed since type inference would require more work 
  | F0OpExp F0Operator [F0Expression symbol typeInfo] -- ^ arithmetic ops, comparison ops, etc. 
  | F0ExpPos SourcePos (F0Expression symbol typeInfo) SourcePos -- ^ Start, Expression, End 

deriving instance (Show (typeInfo F0Type), Show symbol) => Show (F0Declaration symbol typeInfo)
deriving instance (Show (typeInfo F0Type), Show symbol) => Show (F0Expression symbol typeInfo)
-- deriving instance (Show (typeInfo F0Type), Show symbol) => Show (F0Pattern symbol typeInfo)

deriving instance (Eq (typeInfo F0Type), Eq symbol) => Eq (F0Declaration symbol typeInfo)
deriving instance (Eq (typeInfo F0Type), Eq symbol) => Eq (F0Expression symbol typeInfo)
-- deriving instance (Eq (typeInfo F0Type), Eq symbol) => Eq (F0Pattern symbol typeInfo)

data F0Literal = 
    F0IntLiteral Integer 
  | F0StringLiteral String 
  | F0BoolLiteral Bool 
  | F0UnitLiteral 
  deriving (Show, Eq)

data F0Operator = 
    Plus 
  | Minus
  | Times 
  | Divide
  | Equals
  | LessThan
  | Not 
  | And 
  | Or
  deriving (Show, Eq)  

type TypeVariable = String 

data F0Type = 
    F0PrimitiveType F0PrimitiveType
  | F0TypeIdent String 
  | F0TypeVariable TypeVariable 
  | F0TypeVariableTuple [TypeVariable]
  | F0TypeCons F0Type F0Type -- t1 t2 e.g. int list 
  | F0Function F0Type F0Type 
  | F0TupleType [F0Type]
  deriving (Show, Eq)

infixr `F0Function`

data F0PrimitiveType = 
    F0IntType 
  | F0StringType 
  | F0BoolType
  | F0UnitType
  deriving (Show, Eq)

-- | Shortcuts (helpful when writing test cases)
f0Int :: Integer -> F0Expression a b 
f0Int = F0Literal . F0IntLiteral 

f0IntT, f0StringT, f0BoolT, f0UnitT :: F0Type 
f0IntT = F0PrimitiveType F0IntType
f0StringT = F0PrimitiveType F0StringType
f0BoolT = F0PrimitiveType F0BoolType
f0UnitT = F0PrimitiveType F0UnitType

operatorAsFunctionType :: F0Operator -> F0Type 
operatorAsFunctionType = \case 
  Not -> f0BoolT `F0Function` f0BoolT
  other -> let input = F0PrimitiveType $ operatorInput other 
               output = F0PrimitiveType $ operatorOutput other 
           in input `F0Function` input `F0Function` output 

operatorInput, operatorOutput :: F0Operator -> F0PrimitiveType
operatorInput = \case 
  Plus -> F0IntType 
  Minus -> F0IntType 
  Times -> F0IntType 
  Divide -> F0IntType 

  Equals -> F0IntType
  LessThan -> F0IntType

  Not -> F0BoolType
  And -> F0BoolType
  Or -> F0BoolType

operatorOutput = \case 
  Plus -> F0IntType 
  Minus -> F0IntType 
  Times -> F0IntType 
  Divide -> F0IntType 

  Equals -> F0BoolType
  LessThan -> F0BoolType

  Not -> F0BoolType
  And -> F0BoolType
  Or -> F0BoolType

printOp :: F0Operator -> String 
printOp = \case 
  Equals -> "=="
  Plus -> "+"
  Minus -> "-"
  Times -> "*"
  Divide -> "/"
  Not -> "-"
  LessThan -> "<"
  And -> "&&"
  Or -> "||"