{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
module F0.Interpret (
  F0Value(..),

  EvalContext,
  bindContext, emptyContext,

  evalExpr
) where

import F0.Parser.AST
import F0.Codegen.Symbolize

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Control.Monad.Reader.Class
import Control.Monad.Reader

-- | 'Leaf' values for evaluation
data F0Value =
    F0LiteralVal F0Literal
  | F0TupleVal (Vector F0Value)
  | F0ClosureVal { varName :: Symbol, context :: EvalContext, body :: F0Expression Symbol Maybe }
  deriving (Show)

pattern IntVal i = F0LiteralVal (F0IntLiteral i)
pattern BoolVal b = F0LiteralVal (F0BoolLiteral b)

type EvalContext = Map Symbol F0Value
type Evaluator = MonadReader EvalContext

bindContext :: Symbol -> F0Value -> EvalContext -> EvalContext
bindContext = Map.insert

emptyContext :: EvalContext
emptyContext = Map.empty

-- runEvaluator :: Evaluator m => m a -> EvalContext -> a
runEvaluator r ctx = runReader r ctx

evalExpr :: MonadReader EvalContext m => F0Expression Symbol Maybe -> m F0Value
evalExpr = \case
  F0Lambda varName _ body -> do
    context <- ask
    return F0ClosureVal{..}

  F0App e1 e2 -> do
    ~F0ClosureVal{..} <- evalExpr e1
    arg <- evalExpr e2

    local (bindContext varName arg) (evalExpr body)

  F0Let (F0Value name _ e) e2 -> do
    val <- evalExpr e
    local (bindContext name val) (evalExpr e2)

  F0Let (F0Fun name args _ e) e2 -> do
    oldContext <- ask
    -- Both contexts will have name = func
    let lambdafy :: F0Expression Symbol Maybe -> [(Symbol, Maybe F0Type)] -> F0Expression Symbol Maybe
        lambdafy base = \case
          [] -> base
          (argName, t) : args -> F0Lambda argName t (lambdafy base args)

        context = bindContext name body oldContext
        body = F0ClosureVal { varName = name, context, body = lambdafy e args }

    local (const context) (evalExpr e2)

  F0Let (F0Data{}) e2 -> do
    error "evalExpr: datatypes not yet implemented"

  F0If test trueBranch falseBranch -> do
    ~(BoolVal b) <- evalExpr test
    evalExpr (if b then trueBranch else falseBranch)

  F0Literal literal -> return $ F0LiteralVal literal
  F0TagValue{} -> error "datatypes not yet implemented"
  F0Case{} -> error "datatypes not yet implemented"
  F0Identifier name -> asks (Map.! name)
  F0Tuple es -> F0TupleVal <$> traverse evalExpr (Vector.fromList es)
  F0TupleAccess i _ e -> do
    ~(F0TupleVal tuple) <- evalExpr e
    return $ tuple Vector.! i

  F0TypeAssertion e _ -> evalExpr e
  F0OpExp opr es -> do
    vals <- traverse evalExpr es
    -- For now just do Plus, Equals, Not, LessThan 
    return $ case opr of
      Plus ->
        let ~[IntVal a, IntVal b] = vals
        in IntVal (a + b)

      Equals ->
        let ~[IntVal a, IntVal b] = vals
        in BoolVal (a == b)

      LessThan ->
        let ~[IntVal a, IntVal b] = vals
        in BoolVal (a < b)

      Not ->
        let ~[BoolVal b] = vals
        in BoolVal (not b)

      _ -> error $ "Not yet implemented: " ++ show opr

  F0ExpPos _ e _ -> evalExpr e