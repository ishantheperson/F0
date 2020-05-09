{-# LANGUAGE LambdaCase #-}
module Parser.ASTUtil where 

import Parser.AST 

import Data.Set (Set)
import qualified Data.Set as Set 

import Data.Map (Map)
import qualified Data.Map.Strict as Map 

import Text.Printf

declName :: F0Declaration symbol typeInfo -> symbol 
declName = \case 
  F0Value n _ _ -> n 
  F0Fun n _ _ _ -> n 
  F0DeclPos _ d _ -> declName d 

-- | Takes an expression, a map from 
-- identifiers to their types (the context at this point)
-- and returns the free variables of an expression and their types
freeVariables :: Ord s => F0Expression s f -> Set s 
freeVariables = \case
  F0Lambda name t e -> Set.singleton name <> freeVariables e 
  F0App e1 e2 -> freeVariables e1 <> freeVariables e2 
  F0Identifier x -> Set.singleton x 
  F0Literal _ -> Set.empty 
  F0OpExp _ es -> Set.unions (freeVariables <$> es)
  F0ExpPos _ e _ -> freeVariables e 
  F0If e1 e2 e3 -> Set.unions (freeVariables <$> [e1, e2, e3])

class TypeSubstitutable a where 
  subst :: Substitution -> a -> a 
  freeTypeVariables :: a -> Set TypeVariable

type Substitution = Map TypeVariable F0Type 
emptySubstitution :: Substitution
emptySubstitution = Map.empty 

-- Apply substitution s1 to s2, and also apply s1 
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.map (subst s1) s2 <> s1 

instance TypeSubstitutable F0Type where 
  subst s = \case 
    t@(F0TypeVariable a) -> Map.findWithDefault t a s 
    F0Function a b -> subst s a `F0Function` subst s b 
    t -> t 

  freeTypeVariables = \case 
    F0TypeVariable a -> Set.singleton a 
    F0Function a b -> freeTypeVariables a <> freeTypeVariables b 
    _ -> Set.empty     

normalizeSubst :: TypeSubstitutable a => a -> Substitution
normalizeSubst t = 
  let vars = Set.toList $ freeTypeVariables t 
      subst = zipWith (\v i -> (v, F0TypeVariable (names !! i))) vars [0..]
  in Map.fromList subst
  where names :: [String]
        names = map pure ['a'..'z'] ++ do 
          i <- [1..]
          j <- ['a'..'z']
          return (j : show i)

printType :: F0Type -> String 
printType t = printType' $ subst (normalizeSubst t) t  
  where printType' :: F0Type -> String 
        printType' = \case 
          F0PrimitiveType p -> printPrimitiveType p 
          F0TypeIdent s -> s 
          F0TypeVariable a -> "'" ++ a 
          F0Function a@(F0Function _ _) b -> printf "(%s) -> %s" (printType' a) (printType' b) 
          F0Function a b -> printf "%s -> %s" (printType' a) (printType' b) 

printPrimitiveType :: F0PrimitiveType -> String 
printPrimitiveType = \case 
  F0IntType -> "int"
  F0StringType -> "string"
  F0BoolType -> "bool"

class RemovablePosition a where  
  removePositionInfo :: a -> a 

instance RemovablePosition (F0Declaration s t) where 
  removePositionInfo (F0DeclPos _ decl _) = removePositionInfo decl 
  removePositionInfo (F0Value name t e) = F0Value name t (removePositionInfo e)
  removePositionInfo (F0Fun name args t e) = F0Fun name args t (removePositionInfo e)

instance RemovablePosition (F0Expression s t) where 
  removePositionInfo (F0ExpPos _ e _) = removePositionInfo e 
  removePositionInfo (F0OpExp op es) = F0OpExp op (removePositionInfo <$> es)
  removePositionInfo (F0TypeAssertion e t) = F0TypeAssertion (removePositionInfo e) t 
  removePositionInfo (F0Lambda name t e) = F0Lambda name t (removePositionInfo e)
  removePositionInfo (F0App e1 e2) = F0App (removePositionInfo e1) (removePositionInfo e2) 
  removePositionInfo (F0If e1 e2 e3) = F0If (removePositionInfo e1) (removePositionInfo e2) (removePositionInfo e3)
  removePositionInfo other = other 

-- | Shortcuts (helpful when writing test cases)
f0Int :: Integer -> F0Expression a b 
f0Int = F0Literal . F0IntLiteral 

f0IntT, f0StringT, f0BoolT :: F0Type 
f0IntT = F0PrimitiveType F0IntType
f0StringT = F0PrimitiveType F0StringType
f0BoolT = F0PrimitiveType F0BoolType