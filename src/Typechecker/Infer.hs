{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
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

data TypeErrorData = Mismatch F0Type F0Type | InfiniteType TypeVariable F0Type deriving (Show, Eq)
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

instance TypeSubstitutable (F0Expression Symbol Identity) where 
  applySubst s = \case 
    F0Lambda x (Identity t) e -> F0Lambda x (Identity $ applySubst s t) (applySubst s e)
    F0App e1 e2 -> F0App (applySubst s e1) (applySubst s e2)
    F0OpExp op es -> F0OpExp op (applySubst s <$> es)
    F0If e1 e2 e3 -> F0If (applySubst s e1) (applySubst s e2) (applySubst s e3)
    F0TypeAssertion e t -> F0TypeAssertion (applySubst s e) (applySubst s t)
    F0ExpPos start e end -> F0ExpPos start (applySubst s e) end 
    other -> other 

  freeTypeVariables = \case 
    F0Lambda x (Identity t) e -> freeTypeVariables t `Set.union` freeTypeVariables e
    F0App e1 e2 -> freeTypeVariables e1 `Set.union` freeTypeVariables e2
    F0OpExp op es -> Set.unions (freeTypeVariables <$> es)
    F0If e1 e2 e3 -> Set.unions (freeTypeVariables <$> [e1, e2, e3])
    F0TypeAssertion e t -> freeTypeVariables t `Set.union` freeTypeVariables e
    F0ExpPos start e end -> freeTypeVariables e 
    other -> Set.empty  

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

infer :: Infer m => TypeEnvironment -> Maybe SourceRange -> F0Expression Symbol Maybe 
                 -> m (F0Expression Symbol Identity, (Substitution, F0Type)) 
infer env range = \case 
  F0Identifier x -> do 
    inferred <- lookupVar env x
    return (F0Identifier x, inferred)

  F0TypeAssertion e t -> do 
    tv <- F0TypeVariable <$> mkFreshVar
    (e, (s1, t1)) <- infer env range e 
    s2 <- unify range t1 t 
    let t = applySubst s2 t1
    return (F0TypeAssertion e t, (s2, t))

  F0Lambda x Nothing e -> do 
    tv <- F0TypeVariable <$> mkFreshVar
    env <- return $ extendEnv env x (Forall [] tv)
    (e, (s1, t1)) <- infer env range e 

    let t = applySubst s1 tv
    return (F0Lambda x (Identity t) e, (s1, F0Function t t1) )

  F0Lambda x (Just t) e -> do 
    env <- return $ extendEnv env x (Forall [] t)
    (e, (s1, t1)) <- infer env range e 

    t <- return $ applySubst s1 t
    return (F0Lambda x (Identity t) e, (s1, F0Function t t1) )

  F0App e1 e2 -> do 
    tv <- F0TypeVariable <$> mkFreshVar
    (e1, (s1, t1)) <- infer env range e1 
    (e2, (s2, t2)) <- infer (applySubst s1 env) range e2 
    s3 <- unify range (applySubst s2 t1) (F0Function t2 tv)
    return (F0App e1 e2, (s3 `composeSubstitution` s2 `composeSubstitution` s1, applySubst s3 tv))

  F0IntLiteral i -> return (F0IntLiteral i, (emptySubstitution, F0PrimitiveType F0IntType))
  F0ExpPos start e end -> do 
    (e, inferred) <- infer env (Just (start, end)) e 
    return (F0ExpPos start e end, inferred)

-- | Returns typechecking errors, or AST with type information inserted as well as the most general type
typecheck :: F0Expression Symbol Maybe -> Either [TypeError] (F0Expression Symbol Identity, F0Type)
typecheck e = case runWriter (evalStateT (infer emptyEnv Nothing e) 0) of 
  ((e, (s, t)), []) -> 
    let e' = applySubst s e -- Insert type information into the expression
        normalizer = normalizeSubst e' -- Rename all type variables in e to a, b, c, ...
    in Right (applySubst normalizer e', applySubst normalizer t)
  (_, errors) -> Left errors


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
