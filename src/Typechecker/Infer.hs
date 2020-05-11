{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Typechecker.Infer where 

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

import Control.Monad.Writer
import Control.Monad.State.Strict

import Text.Printf 
import Text.Show.Pretty

import GHC.Stack
import Debug.Trace

-- Based off of: http://dev.stephendiehl.com/fun/006_hindley_milner.html

data Scheme = Forall [TypeVariable] F0Type deriving (Show, Eq)
newtype TypeEnvironment = TypeEnvironment (Map Symbol Scheme) deriving Show 

emptyEnv :: TypeEnvironment
emptyEnv = TypeEnvironment Map.empty 

extendEnv :: TypeEnvironment -> Symbol -> Scheme -> TypeEnvironment
extendEnv (TypeEnvironment env) name t = TypeEnvironment $ Map.insert name t env 

extendEnvs :: TypeEnvironment -> [(Symbol, Scheme)] -> TypeEnvironment
extendEnvs env = foldl (\e (sym, scheme) -> extendEnv e sym scheme) env  

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
type TypeError = (Maybe SourceRange, TypeErrorData)

printTypeError :: TypeError -> String 
printTypeError (range, (Mismatch a b)) = 
  printf "%s: couldn't match type '%s' with '%s'" (printSourceRange range) (display a) (display b)
printTypeError (range, (InfiniteType a b)) =
  printf "%s: found infinite type when trying to solve '%s ~ %s'" (printSourceRange range) a (display b)

data InferState = InferState 
  {
    uniqueCount :: Int,
    constructors :: Map Symbol (String, [(Symbol, Scheme)])
  }
  deriving (Show)

defaultState :: InferState
defaultState = InferState 0 Map.empty 

type Infer m = (MonadState InferState m, MonadWriter [TypeError] m)

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
    F0Let d e -> F0Let d (subst s e)
    F0Tuple es -> F0Tuple (subst s es)
    F0TupleAccess i n e -> F0TupleAccess i n (subst s e)
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
    other -> Set.empty  

-- | There is no way we can unify a ~ b if a appears in b
-- e.g. a ~ a -> b
occursCheck :: TypeSubstitutable a => TypeVariable -> a -> Bool
occursCheck a t = a `Set.member` freeTypeVariables t 

unify :: Infer m => Maybe SourceRange -> F0Type -> F0Type -> m Substitution
unify range (F0TypeTuple [a]) b = unify range a b -- Remove type tuple layer if there's only one item
unify range a (F0TypeTuple [b]) = unify range a b  
unify range (F0TypeVariable a) t = bind range a t 
unify range t (F0TypeVariable a) = bind range a t
unify range (F0PrimitiveType a) (F0PrimitiveType b) | a == b = return emptySubstitution 
unify range (a `F0Function` b) (c `F0Function` d) = do 
  s1 <- unify range a c 
  s2 <- unify range (subst s1 b) (subst s1 d)
  return $ s1 `composeSubst` s2 
unify range (F0TupleType t1s) (F0TupleType t2s) | length t1s == length t2s = do 
  s1 <- foldM (\s (t1, t2) -> do s' <- unify range t1 t2; return $ s' `composeSubst` s) emptySubstitution (zip t1s t2s)
  return s1 

unify range (F0TypeCons t1 a) (F0TypeCons t2 b) | a == b = unify range t1 t2 
unify range (F0TypeTuple t1s) (F0TypeTuple t2s) | length t1s == length t2s = do 
  s1 <- foldM (\s (t1, t2) -> do s' <- unify range t1 t2; return $ s' `composeSubst` s) emptySubstitution (zip t1s t2s)
  return s1 

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
    Nothing -> 
      case v of 
        NativeFunction n -> do 
          let C0LibraryBinding _ t = libraryBindings Map.! n 
          return (emptySubstitution, t)
        _ -> error $ "Unexpected unbound variable (should've been found in symbolization): " ++ show v 

infer :: forall m. (HasCallStack, Infer m) => TypeEnvironment -> Maybe SourceRange -> F0Expression Symbol Maybe 
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
    return (F0Lambda x (Identity t) e, (s1, F0Function t (subst s1 t1)))

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

  F0OpExp Not [e1] -> do 
    (e1, (s1, t1)) <- infer env range e1 
    s2 <- unify range t1 f0BoolT
    return (F0OpExp Not [e1], (s2 `composeSubst` s1, f0BoolT))

  F0OpExp op [e1, e2] -> do 
    (e1, (s1, t1)) <- infer env range e1 
    (e2, (s2, t2)) <- infer (subst s1 env) range e2 

    tv <- F0TypeVariable <$> freshName

    s3 <- unify range (t1 `F0Function` t2 `F0Function` tv) (operatorAsFunctionType op)
    return (F0OpExp op [e1, e2], (s1 `composeSubst` s2 `composeSubst` s3, subst s3 tv))

  F0OpExp _ _ -> error "infer: invalid operator combination!"

  F0Tuple es -> do 
    (s, ts, es) <- inferMany emptySubstitution es
    return (F0Tuple es, (s, subst s (F0TupleType ts)))

  F0TupleAccess i n e -> do 
    (e1, (s1, t1)) <- infer env range e 
    tvs <- replicateM n (F0TypeVariable <$> freshName)
    s2 <- unify range (F0TupleType tvs) t1 
    
    return (F0TupleAccess i n e1, (s2 `composeSubst` s1, getNth (subst s2 t1) i))
    where getNth (F0TupleType ts) i = ts !! i 
          getNth _ _ = error "should be a tuple type by now"

  F0Let d e -> do 
    (ds, s, ts) <- inferDecl env range d -- inferDecl may return a substitution
    (e, (s2, t2)) <- infer (subst s $ extendEnvs env (declNames ds ts)) range e 
    return (foldr F0Let e ds, (s2 `composeSubst` s, t2))

  F0Case obj rules -> do 
    -- Check all labels are from the same type
    let actualLabels = sort $ fst <$> rules 
    (tycon, labelsWithTypes) <- getConstructors (fst $ head rules)
    let realLabels = sort $ fst <$> labelsWithTypes

    if actualLabels /= realLabels then error "Match nonexhaustive or invalid constructor"
    else do 
      let Forall tvs _ = snd . head $ labelsWithTypes
      -- All branches of the case need to have their tvs instantiated to the same thing
      names <- mapM (const $ F0TypeVariable <$> freshName) tvs 
      let ruleSubst :: Substitution
          ruleSubst = Map.fromList (zip tvs names)
          -- The bound variable for all branches should now be (Forall [] (subst ruleSubst t))
          schemes = map (\(sym, schm) -> (sym, subst ruleSubst $ unbind schm)) labelsWithTypes

      let branchInfo = flip map rules $ \(sym, branch) -> (fromJust $ lookup sym schemes, branch)
      (env, s, unzip -> (branches, ts)) <- inferBranches env branchInfo
      -- Unify all the types in branches 
      caseTyv <- F0TypeVariable <$> freshName
      let caseType = F0TupleType $ replicate (length branches) caseTyv
      s2 <- unify range caseType (F0TupleType ts)
      
      -- At this point we want to return environment = subst s2 env, t = subst s2 caseTyv (or maybe the head of ts?)
      -- But we also need to make sure the object has type 
      -- F0Cons (F0TypeTuple names) tycon 

      (obj, (s3, tobj)) <- infer (subst s2 env) range obj 
      s4 <- unify range (F0TypeCons (F0TypeTuple names) tycon) tobj 

      let branches' = zipWith (\(constr, (x, _)) e -> (constr, (x, e))) rules branches 
      return (F0Case obj branches', (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s, subst s2 (head ts)))

    -- TODO: switch to ExceptT monad since recovering in this situation is hard

    where inferBranches :: TypeEnvironment -> [(Scheme, (Symbol, F0Expression Symbol Maybe))] 
                        -> m (TypeEnvironment, Substitution, [(F0Expression Symbol Identity, F0Type)])
          inferBranches env = \case 
            [] -> return (env, emptySubstitution, [])
            (scheme, (x, e)):schemes -> do 
              (env, s, ts) <- inferBranches env schemes 
              (e, (s2, t)) <- infer (extendEnv env x scheme) range e 
              return (subst s2 env, s2 `composeSubst` s, (e, t):ts)

          unbind :: Scheme -> Scheme 
          unbind (Forall tvs t) = Forall [] t 

  F0If e1 e2 e3 -> do 
    (e1, (s1, t1)) <- infer env range e1 
    (e2, (s2, t2)) <- infer (subst s1 env) range e2 
    (e3, (s3, t3)) <- infer (subst (s2 `composeSubst` s1) env) range e3 -- Should this substitute s2 `compose` s1? 

    s4 <- unify range t1 f0BoolT
    s5 <- unify range t2 t3 -- Need to substitute here?
    return (F0If e1 e2 e3, (s5 `composeSubst` s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, subst s5 t2))

  F0Literal (F0IntLiteral i) -> return (F0Literal (F0IntLiteral i), (emptySubstitution, f0IntT))
  F0Literal (F0StringLiteral s) -> return (F0Literal (F0StringLiteral s), (emptySubstitution, f0StringT))
  F0Literal (F0BoolLiteral b) -> return (F0Literal (F0BoolLiteral b), (emptySubstitution, f0BoolT))
  F0Literal F0UnitLiteral -> return (F0Literal F0UnitLiteral, (emptySubstitution, f0UnitT))

  F0ExpPos start e end -> do 
    (e, inferred) <- infer env (Just (start, end)) e 
    return (F0ExpPos start e end, inferred)

  where inferMany :: Infer m => Substitution -> [F0Expression Symbol Maybe] 
                             -> m (Substitution, [F0Type], [F0Expression Symbol Identity])
        inferMany s [] = return (s, [], [])
        inferMany s (e:es) = do 
          (s1, ts, es) <- inferMany s es 
          (e, (s2, t)) <- infer (subst s1 env) range e 
          return (s2 `composeSubst` s1, t:ts, e:es) 

inferDecl :: (HasCallStack, Infer m) => TypeEnvironment -> Maybe SourceRange -> F0Declaration Symbol Maybe 
                     -> m ([F0Declaration Symbol Identity], Substitution, [Scheme])
inferDecl env range = \case                      
  F0Value name _ e -> do 
    (e, (s, t)) <- infer env range e 
    t <- return $ subst s t 
    let scheme = generalize (subst s env) t 

    return ([F0Value name (Identity t) e], s, [scheme])

  F0Fun name args _ e -> do 
    let lambdafied = lambdafy e args 

    recursiveT <- F0TypeVariable <$> freshName 
    env <- return $ extendEnv env name (Forall [] recursiveT)
    -- Bind name to "recursiveT"
    (e, (s, t)) <- infer env range lambdafied 
    s2 <- unify range t (subst s recursiveT)

    -- traceM (ppShow (subst s recursiveT))
    -- traceM (ppShow s)
    -- traceM (ppShow s2)
    -- traceM (ppShow $ (subst (s `composeSubst` s2) recursiveT) )

    -- -- t <- return $ subst s t 
    -- -- recursiveT <- return $ subst s t 

    -- -- ftS <- unify Nothing t recursiveT 
    -- -- t <- return $ subst ftS t 

    let scheme = generalize env (subst s2 $ subst s t) 
    return ([F0Value name (Identity t) e], s `composeSubst` s2, [scheme])

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
    return ((F0Data tvs name constructors):constructorFunctions, -- Don't forget about the original data declaration
            emptySubstitution, 
            constructionSchemes)

  F0DeclPos start d end -> inferDecl env (Just (start, end)) d 

inferDecls :: Infer m => TypeEnvironment -> [F0Declaration Symbol Maybe] 
                      -> m ([F0Declaration Symbol Identity], TypeEnvironment)
inferDecls env = \case 
  [] -> return ([], env)
  d : ds -> do 
    (newDecls, _, schemes) <- inferDecl env Nothing d -- I think we can ignore the substitution here since everything else has been generalized
    env <- return $ extendEnvs env (declNames newDecls schemes)
    (symbols, env) <- inferDecls env ds 
    return (newDecls ++ symbols, env)

-- | Returns typechecking errors, or AST with type information inserted as well as the most general type
typecheck :: TypeEnvironment -> F0Expression Symbol Maybe -> Either [TypeError] (F0Expression Symbol Identity, Scheme)
typecheck env e = case runWriter (evalStateT (infer env Nothing e) defaultState) of 
  ((e, (s, t)), []) -> 
    let e' = subst s e -- Insert type information into the expression
        normalizer = normalizeSubst e' -- Rename all type variables in e to a, b, c, ...
    in Right (subst normalizer e', generalize emptyEnv $ subst (normalizer `composeSubst` s) t)
  (_, errors) -> Left errors

typecheckDecls :: TypeEnvironment -> [F0Declaration Symbol Maybe] -> Either [TypeError] ([F0Declaration Symbol Identity], TypeEnvironment)
typecheckDecls env decls = 
  case runWriter (evalStateT (inferDecls env decls) defaultState) of 
    (results, []) -> Right results 
    (_, errors) -> Left errors 
