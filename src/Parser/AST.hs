{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.AST where 

import Text.Megaparsec.Pos 
import Text.Printf

import Data.Functor.Foldable.TH

import Display

type SourceRange = (SourcePos, SourcePos)

instance Display (Maybe SourceRange) where 
  display = printSourceRange
    where printSourceRange Nothing = "<no position information available>"
          printSourceRange (Just (start, end)) = sourcePosPretty start ++ " - " ++ sourcePosPretty end 

  displayIO Nothing = pure "<no position information available>"
  displayIO (Just (start, end)) = do 
    -- Megaparsec source positions are indexed from 1 
    -- Also show one more line before for context
    let SourcePos fname (pred . unPos -> startLine) (pred . unPos -> startCol) = start 
        SourcePos _ (pred . unPos -> endLine) (pred . unPos -> endCol) = end 
    
    fileLines <- lines <$> readFile fname 
    let sourceTxt = if startLine == endLine 
                      then showRange (fileLines !! startLine) (succ startLine) startCol endCol
                      else
                        let startLineTxt = fileLines !! startLine 
                            otherLinesTxt = take (endLine - startLine) $ drop (startLine + 1) fileLines
                        in  
                          showRange startLineTxt (unPos $ sourceLine start) startCol (length startLineTxt) ++ "\n"
                            ++ showRanges (succ . unPos $ sourceLine start) endCol otherLinesTxt

    return $ printf "%s: %d.%d - %d.%d:\n\n%s%s\n" fname (unPos $ sourceLine start) (unPos $ sourceColumn start)
                                                   (unPos $ sourceLine end) (unPos $ sourceColumn end)
                                                   (if startLine == 0 then ""
                                                    else showRange (fileLines !! (startLine - 1)) startLine 0 0 ++ "\n")
                                                   sourceTxt                      
        
    where green = "\x1b[32m"
          boldRed = "\x1b[1m\x1b[31m"
          reset = "\x1b[0m"
          predZ x = if x == 0 then 0 else pred x  

          showRange :: String -> Int -> Int -> Int -> String
          showRange line num start end = 
            let 
              front = take start line 
              mid = take (end - start) $ drop start line 
              back = drop end line 
            in 
              printf "%s%4d |%s %s%s%s%s%s" green num reset front boldRed mid reset back 

          showRanges :: Int -> Int -> [String] -> String
          showRanges lineNum end = \case 
            [] -> error "showRanges: empty argument"
            [line] -> showRange line lineNum 0 end 
            x:xs -> showRange x lineNum 0 (length x) ++ "\n" ++ showRanges (succ lineNum) end xs 

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
    F0Lambda symbol (typeInfo F0Type) (F0Expression symbol typeInfo) -- ^ fn x (: t) => etc 
  | F0App (F0Expression symbol typeInfo) (F0Expression symbol typeInfo) -- ^ e1 e2 
  | F0Let (F0Declaration symbol typeInfo) (F0Expression symbol typeInfo) -- ^ let decl in e end. Multiple decls become nested lets
  | F0If (F0Expression symbol typeInfo) (F0Expression symbol typeInfo) (F0Expression symbol typeInfo) -- ^ if e1 then e2 else e3 
  | F0Literal F0Literal 
  | F0TagValue String Int (F0Expression symbol typeInfo) -- ^ introduce sum type
  | F0Case (F0Expression symbol typeInfo) [(symbol, (symbol, F0Expression symbol typeInfo))] -- ^ rules are <constructor> <bound var> <e>
  | F0Identifier symbol 
  | F0Tuple [F0Expression symbol typeInfo] -- ^ Construct a tuple
  | F0TupleAccess Int Int (F0Expression symbol typeInfo) -- ^ Access element i out of n in e 
  | F0TypeAssertion (F0Expression symbol typeInfo) F0Type -- Removed since type inference would require more work 
  | F0OpExp F0Operator [F0Expression symbol typeInfo] -- ^ arithmetic ops, comparison ops, etc. 
  | F0ExpPos SourcePos (F0Expression symbol typeInfo) SourcePos -- ^ Start, Expression, End 

deriving instance (Show (typeInfo F0Type), Show symbol) => Show (F0Declaration symbol typeInfo)
deriving instance (Show (typeInfo F0Type), Show symbol) => Show (F0Expression symbol typeInfo)

deriving instance (Eq (typeInfo F0Type), Eq symbol) => Eq (F0Declaration symbol typeInfo)
deriving instance (Eq (typeInfo F0Type), Eq symbol) => Eq (F0Expression symbol typeInfo)

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
  | Mod
  | Equals
  | NotEquals
  | LessThan
  | LessEq
  | GreaterThan
  | GreaterEq  
  | Not 
  | And 
  | Or
  deriving (Show, Eq)  

type TypeVariable = String 

data F0Type = 
    F0PrimitiveType F0PrimitiveType
  | F0TypeVariable TypeVariable 
  | F0TypeTuple [F0Type] -- ^ e.g. (int, string) either. Can have any number of elements (even 0 or 1)
  | F0TypeCons F0Type String -- ^ t1 t2 e.g. int list. Right side must be the name of a datatype
  | F0Function F0Type F0Type 
  | F0TupleType [F0Type] -- ^ e.g. (string * int) list. MUST have more than 1 element
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
  Mod -> F0IntType

  Equals -> F0IntType
  NotEquals -> F0IntType
  LessThan -> F0IntType
  LessEq -> F0IntType
  GreaterThan -> F0IntType
  GreaterEq -> F0IntType

  Not -> F0BoolType
  And -> F0BoolType
  Or -> F0BoolType

operatorOutput = \case 
  Plus -> F0IntType 
  Minus -> F0IntType 
  Times -> F0IntType 
  Divide -> F0IntType 
  Mod -> F0IntType

  Equals -> F0BoolType
  NotEquals -> F0BoolType
  LessThan -> F0BoolType
  LessEq -> F0BoolType
  GreaterThan -> F0BoolType
  GreaterEq -> F0BoolType

  Not -> F0BoolType
  And -> F0BoolType
  Or -> F0BoolType

literalType :: F0Literal -> F0Type 
literalType = \case
  F0IntLiteral _ -> f0IntT
  F0StringLiteral _ -> f0StringT
  F0BoolLiteral _ -> f0BoolT
  F0UnitLiteral -> f0UnitT

printOp :: F0Operator -> String 
printOp = \case 
  Equals -> "=="
  NotEquals -> "!="
  Plus -> "+"
  Minus -> "-"
  Times -> "*"
  Divide -> "/"
  Mod -> "%"
  Not -> "-"
  LessThan -> "<"
  LessEq -> "<="
  GreaterThan -> ">"
  GreaterEq -> ">="
  And -> "&&"
  Or -> "||"

makeBaseFunctor ''F0Expression
