{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
module F0.Parser.ASTUtil where 

import F0.Parser.AST 

import Data.List (intercalate)
import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set 

import Data.Map (Map)
import qualified Data.Map.Strict as Map 

import Data.Foldable (fold)
import Data.Functor.Foldable hiding (fold)

import Text.Printf
import F0.Display

declName :: F0Declaration symbol typeInfo -> Maybe symbol 
declName = \case 
  F0Value n _ _ -> Just n 
  F0Fun n _ _ _ -> Just n 
  F0Data{} -> Nothing
  F0DeclPos _ d _ -> declName d 

declNames :: [F0Declaration symbol typeInfo] -> [b] -> [(symbol, b)]
declNames = zip . mapMaybe declName

-- | Takes an expression, a map from 
-- identifiers to their types (the context at this point)
-- and returns the free variables of an expression and their types
freeVariables :: Ord symbol => F0Expression symbol typeInfo -> Set symbol
freeVariables = cata go
  where go (F0IdentifierF x) = Set.singleton x 
        go (F0LambdaF x _ e) = Set.delete x e
        go (F0LetF d e) = 
          case declName d of 
            -- This is a datatype declaration so it 
            -- can't have any free vars
            Nothing -> e 
            Just x -> Set.delete x e <> freeVariablesDecl d
        go (F0CaseF obj rules) = fold $ obj : map (\(_, (x, s)) -> Set.delete x s) rules
        go other = fold other

        freeVariablesDecl = \case 
          F0Value _ _ e -> freeVariables e 
          F0Fun x _ _ e -> Set.delete x (freeVariables e)
          F0Data {} -> Set.empty
          F0DeclPos _ d _ -> freeVariablesDecl d 

type Substitution = Map TypeVariable F0Type 
emptySubstitution :: Substitution
emptySubstitution = Map.empty 

-- Apply substitution s1 to s2, and also apply s1 
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.map (subst s1) s2 <> s1 

class TypeSubstitutable a where 
  subst :: Substitution -> a -> a 
  freeTypeVariables :: a -> Set TypeVariable

instance TypeSubstitutable a => TypeSubstitutable [a] where 
  subst = fmap . subst
  freeTypeVariables = foldr (Set.union . freeTypeVariables) Set.empty 

instance TypeSubstitutable F0Type where 
  subst s = \case 
    t@(F0TypeVariable a) -> Map.findWithDefault t a s 
    F0Function a b -> subst s a `F0Function` subst s b 
    F0TupleType ts -> F0TupleType (subst s <$> ts)
    F0TypeTuple ts -> F0TypeTuple (subst s <$> ts)
    F0TypeCons t n -> F0TypeCons (subst s t) n 
    t@(F0PrimitiveType _) -> t 

  freeTypeVariables = \case 
    F0TypeVariable a -> Set.singleton a 
    F0Function a b -> freeTypeVariables a <> freeTypeVariables b 
    F0TupleType ts -> Set.unions (freeTypeVariables <$> ts)
    F0TypeTuple ts -> Set.unions (freeTypeVariables <$> ts)
    F0TypeCons t _ -> freeTypeVariables t
    _ -> Set.empty     

normalizeSubst :: TypeSubstitutable a => a -> Substitution
normalizeSubst t = 
  let vars = Set.toList $ freeTypeVariables t 
      sub = zipWith (\v i -> (v, F0TypeVariable (names !! i))) vars [0..]
  in Map.fromList sub
  where names :: [String]
        names = map pure ['a'..'z'] ++ do 
          i <- [1..] :: [Int]
          j <- ['a'..'z']
          return (j : show i)

instance Display F0Type where 
  display = printType

-- | Prints out a type, replacing all type variables with 'a, 'b, etc. 
printType :: F0Type -> String 
printType t = printType' $ subst (normalizeSubst t) t  

-- | Prints out a type without normalizing the type variables
printType' :: F0Type -> String 
printType' = \case 
  F0PrimitiveType p -> cyan ++ display p ++ reset
  F0TypeVariable a -> "'" ++ a 
  F0Function a@(F0Function _ _) b -> printf "(%s) -> %s" (printType' a) (printType' b) 
  F0Function a b -> printf "%s -> %s" (printType' a) (printType' b) 
  F0TupleType ts -> "(" ++ intercalate " * " (map printType' ts) ++ ")"
  F0TypeTuple [t] -> printType' t
  F0TypeTuple ts -> "(" ++ intercalate ", " (map printType' ts) ++ ")"
  F0TypeCons (F0TypeTuple []) n -> green ++ n ++ reset 
  F0TypeCons t1 n -> printf "%s %s" (printType' t1) (green ++ n ++ reset) 

  where cyan = "\x1b[36m"
        reset = "\x1b[0m"
        green = "\x1b[32m"

instance Display F0PrimitiveType where 
  display = printPrimitiveType

printPrimitiveType, printPrimitiveTypeC0 :: F0PrimitiveType -> String 
printPrimitiveType = \case 
  F0IntType -> "int"
  F0StringType -> "string"
  F0BoolType -> "bool"
  F0CharType -> "char"
  F0UnitType -> "unit"

-- | This version maps unit as a C0 type
printPrimitiveTypeC0 = \case 
  F0UnitType -> "bool"
  other -> printPrimitiveType other

class RemovablePosition a where  
  removePositionInfo :: a -> a 

instance RemovablePosition (F0Declaration s t) where 
  removePositionInfo (F0DeclPos _ decl _) = removePositionInfo decl 
  removePositionInfo (F0Value name t e) = F0Value name t (removePositionInfo e)
  removePositionInfo (F0Fun name args t e) = F0Fun name args t (removePositionInfo e)
  removePositionInfo (d@(F0Data {})) = d 

instance RemovablePosition (F0Expression s t) where 
  removePositionInfo = cata go
    where go :: F0ExpressionF s t (F0Expression s t) -> F0Expression s t
          go (F0ExpPosF _ e _) = e
          go e = embed e -- Transform from functorized version to regular version

instance RemovablePosition a => RemovablePosition [a] where 
  removePositionInfo = fmap removePositionInfo