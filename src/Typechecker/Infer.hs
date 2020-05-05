{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
module Typechecker.Infer where 

import Parser.AST 
import Parser.ASTUtil 

import Codegen.Symbolize (Symbol(..))

import Data.Functor.Identity

import Data.Set (Set)
import qualified Data.Set as Set 
import Data.Map (Map)
import qualified Data.Map.Strict as Map 

import Control.Monad.Writer
import Control.Monad.State.Strict

import Debug.Trace 

-- Based off of: http://dev.stephendiehl.com/fun/006_hindley_milner.html

data Scheme = Forall [TypeVariable] F0Type deriving Show 
newtype TypeEnvironment = TypeEnvironment (Map Symbol Scheme) deriving Show 

emptyEnv :: TypeEnvironment
emptyEnv = TypeEnvironment Map.empty 

extendEnv :: TypeEnvironment -> Symbol -> Scheme -> TypeEnvironment
extendEnv (TypeEnvironment env) name t = TypeEnvironment $ Map.insert name t env 

data TypeErrorData = Mismatch F0Type F0Type | InfiniteType TypeVariable F0Type deriving Show
type TypeError = (Maybe SourceRange, TypeErrorData)

type Infer m = (MonadState Int m, MonadWriter [TypeError] m)

mkFreshVar :: Infer m => m TypeVariable
mkFreshVar = do 
  i <- get 
  modify succ 
  return $ ("_x" ++ show i)

runInfer = undefined 
{-
  case runWriter $ evalStateT (go Map.empty decls) 0 of 
    (symbolizedDecls, []) -> Right symbolizedDecls
    (_, errors) -> Left errors 
-}

type Substitution = Map TypeVariable F0Type 
emptySubstitution = Map.empty 

-- Apply substitution s1 to s2, and also apply s1 
composeSubstitution s1 s2 = Map.map (applySubst s1) s2 `Map.union` s1 

class TypeSubstitutable a where 
  applySubst :: Substitution -> a -> a 
  freeTypeVariables :: a -> Set TypeVariable

instance TypeSubstitutable F0Type where 
  applySubst s = \case 
    t@(F0TypeVariable a) -> Map.findWithDefault t a s 
    F0Function a b -> applySubst s a `F0Function` applySubst s b 
    t -> t 

  freeTypeVariables = \case 
    F0TypeVariable a -> Set.singleton a 
    F0Function a b -> freeTypeVariables a `Set.union` freeTypeVariables b 
    _ -> Set.empty 

instance TypeSubstitutable Scheme where 
  applySubst s (Forall vars t) = Forall vars $ applySubst s' t 
    where s' = foldr Map.delete s vars 

  freeTypeVariables (Forall vars t) = freeTypeVariables t `Set.difference` Set.fromList vars 

instance TypeSubstitutable a => TypeSubstitutable [a] where 
  applySubst = fmap . applySubst
  freeTypeVariables = foldr (Set.union . freeTypeVariables) Set.empty 

instance TypeSubstitutable TypeEnvironment where 
  applySubst s (TypeEnvironment env) = TypeEnvironment $ Map.map (applySubst s) env 
  freeTypeVariables (TypeEnvironment env) = freeTypeVariables (Map.elems env)

-- | There is no way we can unify a ~ b if a appears in b
-- e.g. a ~ a -> b
occursCheck :: TypeSubstitutable a => TypeVariable -> a -> Bool
occursCheck a t = a `Set.member` freeTypeVariables t 

unify :: Infer m => Maybe SourceRange -> F0Type -> F0Type -> m Substitution
unify range (F0TypeVariable a) t = bind range a t 
unify range t (F0TypeVariable a) = bind range a t
unify range (F0PrimitiveType a) (F0PrimitiveType b) | a == b = return $ emptySubstitution 
unify range (a `F0Function` b) (c `F0Function` d) = do 
  s1 <- unify range a c 
  s2 <- unify range (applySubst s1 b) (applySubst s1 d)
  return $ s1 `composeSubstitution` s2 
unify range t1 t2 = do 
  tell [(range, Mismatch t1 t2)]
  return $ emptySubstitution 

bind :: Infer m => Maybe SourceRange -> TypeVariable -> F0Type -> m Substitution
bind range a t | t == (F0TypeVariable a) = return emptySubstitution -- a and t are the same variable
               | occursCheck a t = do 
                    tell [(range, InfiniteType a t)]
                    return emptySubstitution
              | otherwise = return $ Map.singleton a t 

-- Removes quantifiers 
instantiate :: Infer m => Scheme -> m F0Type 
instantiate (Forall vars t) = do
  names <-  mapM (const $ F0TypeVariable <$> mkFreshVar) vars 
  let subst = Map.fromList (zip vars names)
  return $ applySubst subst t 

generalize :: TypeEnvironment -> F0Type -> Scheme 
generalize env t = Forall vars t 
  where vars = Set.toList $ freeTypeVariables t `Set.difference` freeTypeVariables env 

lookupVar :: Infer m => TypeEnvironment -> Symbol -> m (Substitution, F0Type)
lookupVar (TypeEnvironment env) v = 
  case Map.lookup v env of 
    Just s -> do t <- instantiate s 
                 return (emptySubstitution, t)
    Nothing -> error "Unexpected unbound variable (should've been found in symbolization)"

infer :: Infer m => TypeEnvironment -> Maybe SourceRange -> F0Expression Symbol Maybe -> m (Substitution, F0Type) 
infer env range = \case 
  F0Identifier x -> lookupVar env x
  F0TypeAssertion e t -> do 
    tv <- F0TypeVariable <$> mkFreshVar
    (s1, t1) <- infer env range e 
    s2 <- unify range t1 t 
    return (s2, applySubst s2 t1)

  F0Lambda x t e -> do 
    tv <- F0TypeVariable <$> mkFreshVar
    env <- return $ extendEnv env x (Forall [] tv)
    (s1, t1) <- infer env range e 
    -- Insert type information for "t" here
    -- Then after the big substitution is calculated,
    -- apply that substitution here 
    traceM ("lambda t: " ++ printType (F0Function (applySubst s1 tv) t1))
    return (s1, F0Function (applySubst s1 tv) t1) 

  F0App e1 e2 -> do 
    tv <- F0TypeVariable <$> mkFreshVar
    (s1, t1) <- infer env range e1 
    (s2, t2) <- infer (applySubst s1 env) range e2 
    s3 <- unify range (applySubst s2 t1) (F0Function t2 tv)
    return (s3 `composeSubstitution` s2 `composeSubstitution` s1, applySubst s3 tv)

  F0IntLiteral _ -> return (emptySubstitution, F0PrimitiveType F0IntType)
  F0ExpPos start e end -> infer env (Just (start, end)) e 

typecheck :: F0Expression Symbol Maybe -> Either [TypeError] F0Type 
typecheck e = case runWriter (evalStateT (infer emptyEnv Nothing e) 0) of 
  ((s, t), []) -> traceShow s (Right t)
  (_, errors) -> Left errors

-- normalize :: F0Type -> F0Type 
-- normalize t = 
--   let vars = Set.toList $ freeTypeVariables t 
--   in undefined 
--   where 