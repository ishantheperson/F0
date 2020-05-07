{-# LANGUAGE LambdaCase #-}
module Parser.ASTUtil where 

import Parser.AST 

import Data.Set (Set)
import qualified Data.Set as Set 

import Data.Map (Map)
import qualified Data.Map.Strict as Map 

-- | Takes an expression, a map from 
-- identifiers to their types (the context at this point)
-- and returns the free variables of an expression and their types
freeVariables :: Ord s => Set s -> F0Expression s f -> Set s 
freeVariables bound = \case
  F0Lambda name t e -> freeVariables (Set.insert name bound) e 
  F0App e1 e2 -> freeVariables bound e1 `Set.union` freeVariables bound e2 
  F0Identifier x -> Set.singleton x 
  F0IntLiteral _ -> Set.empty 
  F0StringLiteral _ -> Set.empty 
  F0OpExp _ es -> Set.unions (freeVariables bound <$> es)
  F0ExpPos _ e _ -> freeVariables bound e 

class TypeSubstitutable a where 
  applySubst :: Substitution -> a -> a 
  freeTypeVariables :: a -> Set TypeVariable

type Substitution = Map TypeVariable F0Type 
emptySubstitution :: Substitution
emptySubstitution = Map.empty 

-- Apply substitution s1 to s2, and also apply s1 
composeSubstitution :: Substitution -> Substitution -> Substitution
composeSubstitution s1 s2 = Map.map (applySubst s1) s2 `Map.union` s1 

instance TypeSubstitutable F0Type where 
  applySubst s = \case 
    t@(F0TypeVariable a) -> Map.findWithDefault t a s 
    F0Function a b -> applySubst s a `F0Function` applySubst s b 
    t -> t 

  freeTypeVariables = \case 
    F0TypeVariable a -> Set.singleton a 
    F0Function a b -> freeTypeVariables a `Set.union` freeTypeVariables b 
    _ -> Set.empty     

normalizeSubst :: TypeSubstitutable a => a -> Substitution
normalizeSubst t = 
  let vars = Set.toList $ freeTypeVariables t 
      subst = map (\(v, i) -> (v, F0TypeVariable (names !! i))) (zip vars [0..])
  in Map.fromList subst
  where names :: [String]
        names = map pure ['a'..'z'] ++ do 
          i <- [1..]
          j <- ['a'..'z']
          return (j : show i)

printType :: F0Type -> String 
printType t = printType' $ applySubst (normalizeSubst t) t  
  where printType' :: F0Type -> String 
        printType' = \case 
          F0PrimitiveType p -> printPrimitiveType p 
          F0TypeIdent s -> s 
          F0TypeVariable a -> "'" ++ a 
          F0Function a b -> concat ["(", printType' a, ") -> (", printType' b, ")" ]

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