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

import Text.Show.Pretty

import Debug.Trace 

-- Based off of: http://dev.stephendiehl.com/fun/006_hindley_milner.html

data Scheme = Forall [TypeVariable] F0Type deriving (Show, Eq)
newtype TypeEnvironment = TypeEnvironment (Map Symbol Scheme) deriving Show 

emptyEnv :: TypeEnvironment
emptyEnv = TypeEnvironment Map.empty 

extendEnv :: TypeEnvironment -> Symbol -> Scheme -> TypeEnvironment
extendEnv (TypeEnvironment env) name t = TypeEnvironment $ Map.insert name t env 

getSymbolType :: TypeEnvironment -> Symbol -> Maybe Scheme 
getSymbolType (TypeEnvironment env) name = Map.lookup name env 

printEnv :: TypeEnvironment -> String 
printEnv (TypeEnvironment e) = unlines $ flip map (Map.toList e) (\((Symbol (_, name)), (Forall _ t)) -> "val " ++ name ++ " : " ++ printType t)

data TypeErrorData = Mismatch F0Type F0Type | InfiniteType TypeVariable F0Type deriving (Show, Eq)
type TypeError = (Maybe SourceRange, TypeErrorData)

type Infer m = (MonadState Int m, MonadWriter [TypeError] m)

freshName :: Infer m => m TypeVariable
freshName = do 
  i <- get 
  modify succ 
  return ("_x" ++ show i)

instance TypeSubstitutable Scheme where 
  subst s (Forall vars t) = Forall vars $ subst s' t 
    where s' = foldr Map.delete s vars 

  freeTypeVariables (Forall vars t) = freeTypeVariables t `Set.difference` Set.fromList vars 

instance TypeSubstitutable a => TypeSubstitutable [a] where 
  subst = fmap . subst
  freeTypeVariables = foldr (Set.union . freeTypeVariables) Set.empty 

instance TypeSubstitutable TypeEnvironment where 
  subst s (TypeEnvironment env) = TypeEnvironment $ Map.map (subst s) env 
  freeTypeVariables (TypeEnvironment env) = freeTypeVariables (Map.elems env)

instance TypeSubstitutable (F0Expression Symbol Identity) where 
  subst s = \case 
    F0Lambda x (Identity t) e -> F0Lambda x (Identity $ subst s t) (subst s e)
    F0App e1 e2 -> F0App (subst s e1) (subst s e2)
    F0OpExp op es -> F0OpExp op (subst s <$> es)
    F0If e1 e2 e3 -> F0If (subst s e1) (subst s e2) (subst s e3)
    F0TypeAssertion e t -> F0TypeAssertion (subst s e) (subst s t)
    F0ExpPos start e end -> F0ExpPos start (subst s e) end 
    other -> other 

  freeTypeVariables = \case 
    F0Lambda x (Identity t) e -> freeTypeVariables t <> freeTypeVariables e
    F0App e1 e2 -> freeTypeVariables e1 <> freeTypeVariables e2
    F0OpExp op es -> Set.unions (freeTypeVariables <$> es)
    F0If e1 e2 e3 -> Set.unions (freeTypeVariables <$> [e1, e2, e3])
    F0TypeAssertion e t -> freeTypeVariables t <> freeTypeVariables e
    F0ExpPos start e end -> freeTypeVariables e 
    other -> Set.empty  

-- | There is no way we can unify a ~ b if a appears in b
-- e.g. a ~ a -> b
occursCheck :: TypeSubstitutable a => TypeVariable -> a -> Bool
occursCheck a t = a `Set.member` freeTypeVariables t 

unify :: Infer m => Maybe SourceRange -> F0Type -> F0Type -> m Substitution
unify range (F0TypeVariable a) t = bind range a t 
unify range t (F0TypeVariable a) = bind range a t
unify range (F0PrimitiveType a) (F0PrimitiveType b) | a == b = return emptySubstitution 
unify range (a `F0Function` b) (c `F0Function` d) = do 
  s1 <- unify range a c 
  s2 <- unify range (subst s1 b) (subst s1 d)
  return $ s1 `composeSubst` s2 
unify range t1 t2 = do 
  tell [(range, Mismatch t1 t2)]
  return emptySubstitution 

-- bind :: Infer m => Maybe SourceRange -> TypeVariable -> F0Type -> m Substitution
bind range a t | t == (F0TypeVariable a) = return emptySubstitution -- a and t are the same variable
               | occursCheck a t = do 
                    tell [(range, InfiniteType a t)]
                    return emptySubstitution
              | otherwise = return $ Map.singleton a t 

-- Removes quantifiers 
instantiate :: Infer m => Scheme -> m F0Type 
instantiate (Forall vars t) = do
  names <-  mapM (const $ F0TypeVariable <$> freshName) vars 
  let freeVarSubst = Map.fromList (zip vars names)
  return $ subst freeVarSubst t 

generalize :: TypeEnvironment -> F0Type -> Scheme 
generalize env t = Forall vars t 
  where vars = Set.toList $ freeTypeVariables t `Set.difference` freeTypeVariables env 

lookupVar :: Infer m => TypeEnvironment -> Symbol -> m (Substitution, F0Type)
lookupVar (TypeEnvironment env) v = 
  case Map.lookup v env of 
    Just s -> do t <- instantiate s 
                 return (emptySubstitution, t)
    Nothing -> error $ "Unexpected unbound variable (should've been found in symbolization): " ++ show v 

infer :: Infer m => TypeEnvironment -> Maybe SourceRange -> F0Expression Symbol Maybe 
                 -> m (F0Expression Symbol Identity, (Substitution, F0Type)) 
infer env range = \case 
  F0Identifier x -> do 
    (s, t) <- lookupVar env x
    return (F0Identifier x, (s, t))

  F0TypeAssertion e t -> do 
    (e, (s1, t1)) <- infer env range e 
    s2 <- unify range t1 t 
    let t = subst s2 t1
    return (F0TypeAssertion e t, (s2, t))

  F0Lambda x Nothing e -> do 
    tv <- F0TypeVariable <$> freshName
    env <- return $ extendEnv env x (Forall [] tv)
    (e, (s1, t1)) <- infer env range e 
    let t = subst s1 tv
    return (F0Lambda x (Identity t) e, (s1, F0Function t t1) )

  F0Lambda x (Just t) e -> do 
    env <- return $ extendEnv env x (generalize env t)
    (e, (s1, t1)) <- infer env range e 

    t <- return $ subst s1 t
    return (F0Lambda x (Identity t) e, (s1, F0Function t t1) )

  F0App e1 e2 -> do 
    tv <- F0TypeVariable <$> freshName
    (e1, (s1, t1)) <- infer env range e1 
    (e2, (s2, t2)) <- infer (subst s1 env) range e2 
    s3 <- unify range (subst s2 t1) (F0Function t2 tv)
    return (F0App e1 e2, (s3 `composeSubst` s2 `composeSubst` s1, subst s3 tv))

  F0OpExp op [e1, e2] -> do 
    (e1, (s1, t1)) <- infer env range e1 
    (e2, (s2, t2)) <- infer env range e2 

    traceShowM t1 
    traceShowM t2 

    tv <- F0TypeVariable <$> freshName

    s3 <- unify range (t1 `F0Function` t2 `F0Function` tv) (F0PrimitiveType F0IntType `F0Function` F0PrimitiveType F0IntType `F0Function` F0PrimitiveType F0IntType)
    return (F0OpExp op [e1, e2], (s1 `composeSubst` s2 `composeSubst` s3, subst s3 tv))

  F0OpExp _ _ -> error "infer: Currently all operators should only have two operands!"

  F0IntLiteral i -> return (F0IntLiteral i, (emptySubstitution, F0PrimitiveType F0IntType))
  F0StringLiteral s -> return (F0StringLiteral s, (emptySubstitution, F0PrimitiveType F0StringType))
  F0ExpPos start e end -> do 
    (e, inferred) <- infer env (Just (start, end)) e 
    return (F0ExpPos start e end, inferred)

inferDecl :: Infer m => TypeEnvironment -> F0Declaration Symbol Maybe 
                     -> m (F0Declaration Symbol Identity, Scheme)
inferDecl env = \case                      
  F0Value name _ e -> do 
    (e, (s, t)) <- infer env Nothing e 
    t <- return $ subst s t 
    let scheme = generalize env t 
    return (F0Value name (Identity t) e, scheme)

  F0Fun name args _ e -> do 
    let lambdafied = lambdafy e args 

    recursiveT <- F0TypeVariable <$> freshName 
    env <- return $ extendEnv env name (Forall [] recursiveT)
    -- Bind name to "recursiveT"
    (e, (s, t)) <- infer env Nothing lambdafied 
    t <- return $ subst s t 
    recursiveT <- return $ subst s t 

    ftS <- unify Nothing t recursiveT 
    t <- return $ subst ftS t 

    let scheme = generalize env t 
    return (F0Value name (Identity t) e, scheme)

  where lambdafy :: F0Expression Symbol Maybe -> [(Symbol, Maybe F0Type)] -> F0Expression Symbol Maybe
        lambdafy base = \case 
          [] -> base 
          (argName, t) : args -> F0Lambda argName t (lambdafy base args)

inferDecls :: Infer m => TypeEnvironment -> [F0Declaration Symbol Maybe] 
                      -> m ([F0Declaration Symbol Identity], TypeEnvironment)
inferDecls env = \case 
  [] -> return ([], env)
  d : ds -> do 
    (typeInfoInserted, scheme) <- inferDecl env d
    env <- return $ extendEnv env (declName d) scheme 
    (symbols, env) <- inferDecls env ds 
    return (typeInfoInserted:symbols, env)

-- | Returns typechecking errors, or AST with type information inserted as well as the most general type
typecheck :: TypeEnvironment -> F0Expression Symbol Maybe -> Either [TypeError] (F0Expression Symbol Identity, Scheme)
typecheck env e = case runWriter (evalStateT (infer env Nothing e) 0) of 
  ((e, (s, t)), []) -> 
    let e' = subst s e -- Insert type information into the expression
        normalizer = normalizeSubst e' -- Rename all type variables in e to a, b, c, ...
    in Right (subst normalizer e', generalize emptyEnv $ subst (normalizer `composeSubst` s) t)
  (_, errors) -> Left errors

typecheckDecls :: TypeEnvironment -> [F0Declaration Symbol Maybe] -> Either [TypeError] ([F0Declaration Symbol Identity], TypeEnvironment)
typecheckDecls env decls = 
  case runWriter (evalStateT (inferDecls env decls) 0) of 
    (results, []) -> Right results 
    (_, errors) -> Left errors 

