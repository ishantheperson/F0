{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Typechecker.Infer (typecheck, typecheckExpr, emptyEnv, defaultState, TypeError(..), Scheme(..), getSymbolType) where 

import Parser.AST 
import Parser.ASTUtil 

import Codegen.Symbolize (Symbol(..))

import LibraryBindings

import Data.Functor.Identity

import Data.Maybe (fromJust)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set 
import Data.Map (Map)
import qualified Data.Map.Strict as Map 

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except

import Text.Printf 
import Text.Show.Pretty

import GHC.Stack
import Debug.Trace

-- Based off of: http://dev.stephendiehl.com/fun/006_hindley_milner.html

data Scheme = Forall [TypeVariable] F0Type deriving (Show, Eq)
newtype TypeEnvironment = TypeEnvironment (Map Symbol Scheme) deriving Show 

emptyEnv :: TypeEnvironment
emptyEnv = TypeEnvironment Map.empty 

extendEnv :: Symbol -> Scheme -> TypeEnvironment -> TypeEnvironment
extendEnv name t (TypeEnvironment env) = TypeEnvironment $ Map.insert name t env 

extendEnvs :: [(Symbol, Scheme)] -> TypeEnvironment -> TypeEnvironment
extendEnvs vals env = foldl (\env (sym, schm) -> extendEnv sym schm env) env vals

getSymbolType :: TypeEnvironment -> Symbol -> Maybe Scheme 
getSymbolType (TypeEnvironment env) name = Map.lookup name env 

instance Display TypeEnvironment where 
  display (TypeEnvironment e) = 
    unlines $ flip map (Map.toList e) (\((Symbol (_, name)), (Forall _ t)) -> "val " ++ name ++ " : " ++ display t)

data TypeErrorData = 
    Mismatch F0Type F0Type 
  | InfiniteType TypeVariable F0Type
  | NotConstructor String 
  | ConstructorsDontMatch
  deriving (Show, Eq)  

newtype TypeError = TypeError (Maybe SourceRange, TypeErrorData) deriving (Show, Eq)

instance Display TypeError where 
  display (TypeError (range, (Mismatch a b))) = 
    printf "%s: couldn't match type %s with %s" (printSourceRange range) (display a) (display b)
  display (TypeError (range, (InfiniteType a b))) =
    printf "%s: found infinite type when trying to solve '%s ~ %s'" (printSourceRange range) a (display b)

data InferState = InferState 
  {
    uniqueCount :: Int,
    constructors :: Map Symbol (String, [(Symbol, Scheme)])
  }
  deriving (Show)

defaultState :: InferState
defaultState = InferState 0 Map.empty 

type Infer m = (MonadState InferState m, MonadError TypeError m, MonadReader TypeEnvironment m)

freshName :: Infer m => m TypeVariable
freshName = do 
  state <- get 
  let i = uniqueCount state 
  put (state { uniqueCount = i + 1 })
  return ("_x" ++ show i)

addConstructors :: Infer m => String -> [(Symbol, Scheme)] -> m () 
addConstructors typeName ctors = 
  forM_ ctors $ \(ctor, _) -> do 
    state <- get 
    let oldMap = constructors state 
    put (state { constructors = Map.insert ctor (typeName, ctors) oldMap })

-- | Given a constructor name, gets the type of the constructor
-- as well as the other constructors of that type 
getConstructors :: Infer m => Symbol -> m (String, [(Symbol, Scheme)])
getConstructors ctor = do 
  ctors <- gets constructors 
  return $ ctors Map.! ctor 

-- | Substituting into a scheme respects which variables are bound in the quantification
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
    F0OpExp op es -> F0OpExp op (subst s es)
    F0If e1 e2 e3 -> F0If (subst s e1) (subst s e2) (subst s e3)
    F0TypeAssertion e t -> F0TypeAssertion (subst s e) (subst s t)
    F0ExpPos start e end -> F0ExpPos start (subst s e) end 
    F0Let d e -> F0Let d (subst s e) -- TODO: substitute into d as well? 
    F0Tuple es -> F0Tuple (subst s es)
    F0TupleAccess i n e -> F0TupleAccess i n (subst s e)
    F0Case obj rules -> F0Case (subst s obj) (map (\(constr, (x, e)) -> (constr, (x, subst s e))) rules)
    other -> other 

  freeTypeVariables = \case 
    F0Lambda x (Identity t) e -> freeTypeVariables t <> freeTypeVariables e
    F0App e1 e2 -> freeTypeVariables e1 <> freeTypeVariables e2
    F0OpExp op es -> Set.unions (freeTypeVariables <$> es)
    F0If e1 e2 e3 -> Set.unions (freeTypeVariables <$> [e1, e2, e3])
    F0TypeAssertion e t -> freeTypeVariables t <> freeTypeVariables e
    F0ExpPos start e end -> freeTypeVariables e 
    F0Let d e -> freeTypeVariables e 
    F0Tuple es -> Set.unions (freeTypeVariables <$> es)
    F0TupleAccess _ _ e -> freeTypeVariables e
    F0Case obj rules -> freeTypeVariables obj <> Set.unions (map (\(_, (_, e)) -> freeTypeVariables e) rules)
    other -> Set.empty  

-- Removes quantifiers 
instantiate :: Infer m => Scheme -> m F0Type 
instantiate (Forall vars t) = do
  names <-  mapM (const $ F0TypeVariable <$> freshName) vars 
  let freeVarSubst = Map.fromList (zip vars names)
  return $ subst freeVarSubst t 

generalize :: TypeEnvironment -> F0Type -> Scheme 
generalize env t = Forall vars t 
  where vars = Set.toList $ freeTypeVariables t `Set.difference` freeTypeVariables env 

lookupVar :: Infer m => Symbol -> m F0Type
lookupVar v = do 
  TypeEnvironment env <- ask 
  case Map.lookup v env of 
    Just s -> instantiate s 
    Nothing -> 
      case v of 
        NativeFunction n -> do 
          let C0LibraryBinding _ t = libraryBindings Map.! n 
          return t
        _ -> error $ "Unexpected unbound variable (should've been found in symbolization): " ++ show v 

-- ---------------------
-- Constraint generation
-- ---------------------

type Constraint = (Maybe SourceRange, F0Type, F0Type)

instance TypeSubstitutable Constraint where 
  subst s (range, t1, t2) = (range, subst s t1, subst s t2)
  freeTypeVariables (_, t1, t2) = freeTypeVariables t1 <> freeTypeVariables t2

infer :: (HasCallStack, Infer m) => Maybe SourceRange -> F0Expression Symbol Maybe 
                                       -> m (F0Expression Symbol Identity, (F0Type, [Constraint]))
infer range e = case e of 
  F0ExpPos start e' end -> do
    (e', t) <- infer (Just (start, end)) e' 
    return (F0ExpPos start e' end, t)

  F0Identifier x -> do 
    t <- lookupVar x 
    return (F0Identifier x, (t, []))

  F0TypeAssertion _ _ -> error "infer: type assertion unsupported"

  F0Lambda x _ e -> do 
    xt <- F0TypeVariable <$> freshName
    (e, (et, cs)) <- local (extendEnv x (Forall [] xt)) $ infer range e 

    return (F0Lambda x (Identity xt) e, (xt `F0Function` et, cs))

  F0App e1 e2 -> do 
    (e1, (e1t, e1cs)) <- infer range e1 
    (e2, (e2t, e2cs)) <- infer range e2 

    resultType <- F0TypeVariable <$> freshName 

    return (F0App e1 e2, (resultType, [(range, e1t, e2t `F0Function` resultType)] ++ e1cs ++ e2cs))
    
  F0Tuple es -> do 
    (unzip -> (es, unzip -> (ts, concat -> cs))) <- mapM (infer range) es
    return (F0Tuple es, (F0TupleType ts, cs))

  F0TupleAccess i n e -> do 
    (e, (t, cs)) <- infer range e 
    tvs <- replicateM n (F0TypeVariable <$> freshName)
    
    return (F0TupleAccess i n e, (tvs !! i, (range, F0TupleType tvs, t) : cs))

  F0TagValue _ _ _ -> error "infer: F0TagValue should not be part of type inference"
  F0Case obj rules -> do 
    -- Check all labels are from the same type
    let actualLabels = Set.fromList $ fst <$> rules 
    (tycon, labelsWithTypes) <- getConstructors (fst $ head rules)
    let realLabels = Set.fromList $ fst <$> labelsWithTypes

    unless (actualLabels `Set.isSubsetOf` realLabels) $ throwError (TypeError (range, ConstructorsDontMatch))
    let Forall tvs _ = snd . head $ labelsWithTypes -- Datatypes cannot be empty so this is safe 
    -- All branches of the case need to have their tvs instantiated to the same thing
    names <- mapM (const $ F0TypeVariable <$> freshName) tvs 
    let -- Substitute the type variables the user wrote in the datatype declaration
        -- with new ones 
        ruleSubst :: Substitution
        ruleSubst = Map.fromList (zip tvs names)

        -- The bound variable for all branches should now be (Forall [] (subst ruleSubst t))
        schemes :: [(Symbol, Scheme)]
        schemes = map (\(sym, schm) -> (sym, subst ruleSubst $ unbind schm)) labelsWithTypes

        -- Each branch can be represented by the scheme of its bound variable,
        -- the name of its bound variable, as well as the expression for it 
        branchInfo :: [(Scheme, (Symbol, F0Expression Symbol Maybe))]
        branchInfo = flip map rules $ \(sym, branch) -> (fromJust $ lookup sym schemes, branch)

    (unzip -> (arms, unzip -> (armTys, concat -> c))) <- 
      mapM (\(t, (x, e)) -> local (extendEnv x t) $ infer range e) branchInfo 

    (obj, (objt, objc)) <- infer range obj 
    -- obj : F0TypeCons (F0TypeTuple names) tycon
    -- Unify all the types in branches 
    
    let armTyConstraints = zipWith (range,,) armTys (drop 1 armTys)
        constraints = [(range, objt, F0TypeCons (F0TypeTuple names) tycon)] ++ armTyConstraints ++ objc ++ c 
    
    return (F0Case obj $ zipWith (\(constr, (x, _)) e -> (constr, (x, e))) rules arms, (armTys !! 0, constraints))

    where unbind :: Scheme -> Scheme 
          unbind (Forall tvs t) = Forall [] t 

  F0OpExp Not [e1] -> do 
    (e1, (t, c)) <- infer range e1 
    return (F0OpExp Not [e1], (f0BoolT, (range, t, f0BoolT):c))

  F0OpExp op [e1, e2] -> do 
    (e1, (t1, c1)) <- infer range e1 
    (e2, (t2, c2)) <- infer range e2 

    return (F0OpExp op [e1, e2], (F0PrimitiveType $ operatorOutput op, 
                                 [(range, t1, F0PrimitiveType $ operatorInput op), (range, t2, F0PrimitiveType $ operatorInput op)] ++ c1 ++ c2))

  F0If e1 e2 e3 -> do 
    (e1, (t1, c1)) <- infer range e1 
    (e2, (t2, c2)) <- infer range e2 
    (e3, (t3, c3)) <- infer range e3 

    return (F0If e1 e2 e3, (t2, [(range, t1, f0BoolT), (range, t2, t3)] ++ c1 ++ c2 ++ c3))

  F0Let d e -> do 
    (ds, schms, (s, c)) <- checkDecl range d 
    (e, (t, c2)) <- local (subst s . extendEnvs schms) $ infer range e 
    return (foldr F0Let e ds, (t, c2 ++ c))


  F0Literal l -> return (F0Literal l, (literalType l, []))

-- runInfer :: TypeEnvironment -> StateT InferState (ReaderT TypeEnvironment (Except TypeError)) a -> Either TypeError a
runInfer :: TypeEnvironment -> InferState -> StateT InferState (ReaderT TypeEnvironment (ExceptT e Identity)) a -> Either e (a, InferState)
runInfer env state = runExcept . flip runReaderT env . flip runStateT state 

-- ----------------------
-- Solving constraints
-- ----------------------

runSolve :: Except TypeError a -> Either TypeError a 
runSolve = runExcept

solve :: MonadError TypeError m => [Constraint] -> m Substitution 
solve [] = return emptySubstitution
solve ((range, t1, t2):cs) = do 
  s <- unify range t1 t2 
  s2 <- solve (subst s cs)
  return $ s2 `composeSubst` s

unify :: MonadError TypeError m => Maybe SourceRange -> F0Type -> F0Type -> m Substitution
unify range (F0TypeTuple [a]) b = unify range a b -- Remove type tuple layer if there's only one item
unify range a (F0TypeTuple [b]) = unify range a b  
unify range (F0TypeVariable a) t = bind range a t 
unify range t (F0TypeVariable a) = bind range a t
unify range (F0PrimitiveType a) (F0PrimitiveType b) | a == b = return emptySubstitution 
unify range (a `F0Function` b) (c `F0Function` d) = unifies range [a, b] [c, d]
unify range (F0TupleType t1s) (F0TupleType t2s) | length t1s == length t2s = do 
  unifies range t1s t2s 

unify range (F0TypeCons t1 a) (F0TypeCons t2 b) | a == b = unify range t1 t2 
unify range (F0TypeTuple t1s) (F0TypeTuple t2s) | length t1s == length t2s = do 
  unifies range t1s t2s

unify range t1 t2 = do 
  throwError $ TypeError (range, Mismatch t1 t2)
  return emptySubstitution 

unifies :: MonadError TypeError m => Maybe SourceRange -> [F0Type] -> [F0Type] -> m Substitution
unifies range [] [] = return emptySubstitution
unifies range (t1:t1s) (t2:t2s) = do 
  s1 <- unify range t1 t2 
  s2 <- unifies range (subst s1 t1s) (subst s1 t2s)
  return $ s2 `composeSubst` s1 

unifies range _ _ = error "unifies: Inputs should always be of the same length!"

-- bind :: Infer m => Maybe SourceRange -> TypeVariable -> F0Type -> m Substitution
bind range a t | t == (F0TypeVariable a) = return emptySubstitution -- a and t are the same variable
               | occursCheck a t = do 
                    throwError $ TypeError (range, InfiniteType a t)
                    return emptySubstitution
              | otherwise = return $ Map.singleton a t 

-- | There is no way we can unify a ~ b if a appears in b
-- e.g. a ~ a -> b
occursCheck :: TypeSubstitutable a => TypeVariable -> a -> Bool
occursCheck a t = a `Set.member` freeTypeVariables t 

-- --------------------
-- Top level inference
-- --------------------

-- Returns 1) new list of decls 2) symbol-scheme pairs to add to the environment
-- ... -> m ([F0Declaration Symbol Identity], [(Symbol, Scheme)])
checkDecl :: Infer m => Maybe SourceRange -> F0Declaration Symbol Maybe 
                     -> m ([F0Declaration Symbol Identity], [(Symbol, Scheme)], (Substitution, [Constraint]))
checkDecl range d = do
  env <- ask
  case d of 
    F0DeclPos start d end -> do 
      (decls, sym, c) <- checkDecl (Just (start, end)) d 
      return (map (\d -> F0DeclPos start d end) decls, sym, c)

    F0Value name _ e -> do 
      (e, (t, cs)) <- infer range e 

      s <- solve cs 

      let vt = subst s t
      return ([F0Value name (Identity vt) $ subst s e], [(name, generalize (subst s env) vt)], (s, cs))

    F0Fun name args _ e -> do 
      funT <- F0TypeVariable <$> freshName 

      let lambdaForm = lambdafy e args 
      (e, (t, cs)) <- local (extendEnv name (Forall [] funT)) $ infer range lambdaForm
      s <- solve ((range, t, funT) : cs) 

      let ft = subst s t 
      return ([F0Value name (Identity ft) $ subst s e], [(name, generalize (subst s env) ft)], (s, cs)) 

      where lambdafy :: F0Expression Symbol Maybe -> [(Symbol, Maybe F0Type)] -> F0Expression Symbol Maybe
            lambdafy base = \case 
              [] -> base 
              (argName, t) : args -> F0Lambda argName t (lambdafy base args)

    F0Data tvs name constructors -> do 
      let (constructorFunctions, constructionSchemes, valSchemes) = 
            unzip3 $ flip map (zip [0..] constructors) $ \(i, (constrName, t)) -> 
                        let ft = t `F0Function` F0TypeCons (F0TypeTuple (F0TypeVariable <$> tvs)) name 
                            argName = Symbol (-1, "_datatype") -- Hack alert lmao 
                            e = F0Lambda argName (Identity t) $ F0TagValue name i (F0Identifier argName)

                        in (F0Value constrName (Identity ft) e, Forall tvs ft, Forall tvs t) 
    
          constructorNames = fst $ unzip constructors 

      addConstructors name (declNames constructorFunctions valSchemes)
      st <- get 

      return ((F0Data tvs name constructors):constructorFunctions, -- Don't forget about the original data declaration
              zip constructorNames constructionSchemes, (emptySubstitution, []))


typecheck :: TypeEnvironment -> InferState -> [F0Declaration Symbol Maybe] -> Either TypeError (TypeEnvironment, [F0Declaration Symbol Identity])
typecheck env _ [] = return (env, [])
typecheck env state (d:ds) = do 
  -- Discard constraints at top level 
  ((newDecls, newPairs, _), state) <- runInfer env state (checkDecl Nothing d)
  
  (env', ds') <- typecheck (extendEnvs newPairs env) state ds 
  return (env', newDecls ++ ds')

typecheckExpr :: F0Expression Symbol Maybe -> Either TypeError F0Type 
typecheckExpr e = fst <$> runInfer emptyEnv defaultState go  
  where go :: Infer m => m F0Type 
        go = do 
          (_, (t, c)) <- infer Nothing e 
          s <- solve c 
          return $ subst s t