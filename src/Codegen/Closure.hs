{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-|
  All lambdas are promoted to "real functions".
  All original top level bindings in the program are
  moved to the main() function and are now closures
  which may capture the earlier decls 
-}
module Codegen.Closure where 

import Codegen.Symbolize

import Parser.AST 
import Parser.ASTUtil 

import Data.Maybe (fromMaybe)

import Data.Set (Set)
import qualified Data.Set as Set 

import Control.Monad.State.Lazy 

import Data.Functor.Identity 

import GHC.Stack 
import Debug.Trace 

-- Using F0PrimitiveType instead 
-- data C0Type = ..

data C0VariableReference = 
    C0ArgumentReference -- ^ This is the function's argument
  | C0ClosureReference Int -- ^ This is something captured in the lambda's closure
  | C0ScopeReference Symbol -- ^ This is something which is in scope (e.g top level bindings)
  | C0RecursiveReference -- ^ Recursion is implemented as passing a function's own closure to itself 
  deriving (Show, Eq)

data C0Expression = 
    C0Box F0PrimitiveType C0Expression -- ^ allocates type and casts to void*
  | C0Unbox F0PrimitiveType C0Expression -- ^ Uncast and dereference to target type
  | C0CallClosure C0Expression C0Expression -- ^ cast closure to f0_closure*, call function pointer with closure + arg
  | C0NativeFn String -- ^ Represents a C0 native fn 
  | C0Op F0Operator [C0Expression] -- ^ unboxes ints, performs operation, reboxes
  | C0If C0Expression C0Expression C0Expression  

  -- | Turning a function into a value. The int identifies which function (as lambdas for example are unnamed)
  -- The ints inside the tuple identify which index in the closure that argument should be written to 
  | C0MakeClosure Int [(Symbol, C0VariableReference, Int)] 
  | C0Identifier C0VariableReference
  | C0Literal C0Literal 
  | C0Declare Symbol C0Expression C0Expression -- ^ declare X as E1 in E2
  deriving (Show, Eq)

type C0Environment = [(Symbol, C0VariableReference)]

data C0Literal = 
    C0IntLiteral Integer 
  | C0StringLiteral String 
  | C0BoolLiteral Bool 
  deriving (Show, Eq)

-- | TODO: Remove C0Environment from C0Function. All the 
-- information is already in the expression  
--
-- The first parameter is a string (doesn't have to be unique)
-- The third parameter is just the function's code.
data C0Function = 
  C0Function (Maybe Symbol) C0Expression
  deriving (Show, Eq)

newtype C0CodegenState = C0CodegenState
  { 
    functionPool :: [C0Function]
  }
  deriving (Show, Eq)

initialCodegenState :: C0CodegenState
initialCodegenState = C0CodegenState [] 

type Codegen = MonadState C0CodegenState

runCodegen :: State C0CodegenState a -> (a, C0CodegenState)
runCodegen = flip runState initialCodegenState

addFunction :: Codegen m => C0Expression -> m Int 
addFunction exp = do 
  C0CodegenState currentState <- get 
  let i = length currentState 
  put $ C0CodegenState (currentState ++ [C0Function Nothing exp])
  return i 

codegenExpr :: Codegen m => C0Environment -> F0Expression Symbol typeInfo -> m C0Expression 
codegenExpr env = \case 
  F0ExpPos _ e _ -> codegenExpr env e 
  F0TypeAssertion e _ -> codegenExpr env e 
  F0Literal l -> case l of 
    F0IntLiteral i -> return $ C0Box F0IntType (C0Literal $ C0IntLiteral i)
    F0StringLiteral s -> return $ C0Box F0StringType (C0Literal $ C0StringLiteral s)
    F0BoolLiteral b -> return $ C0Box F0BoolType (C0Literal $ C0BoolLiteral b)
    F0UnitLiteral -> return $ C0Box F0IntType (C0Literal $ C0IntLiteral 0)

  F0Identifier (NativeFunction n) -> do 
    return $ C0NativeFn n 

  F0Identifier x -> do 
    return $ C0Identifier (forceLookup x env)

  F0App e1 e2 -> do 
    f <- codegenExpr env e1 
    e <- codegenExpr env e2 
    return $ C0CallClosure f e 

  F0OpExp Not [e1] -> do 
    a <- codegenExpr env e1 
    return $ C0Box F0BoolType $ C0Op Not [C0Unbox F0BoolType a]

  F0OpExp op [e1, e2] -> do 
    a <- codegenExpr env e1
    b <- codegenExpr env e2 

    return $ C0Box (operatorOutput op) $ C0Op op [C0Unbox (operatorInput op) a, C0Unbox (operatorInput op) b]

  F0OpExp _ _ -> error "codegenExpr: All operators should only have 2 operands at present"

  F0If e1 e2 e3 -> do 
    test <- codegenExpr env e1 
    trueBranch <- codegenExpr env e2 
    falseBranch <- codegenExpr env e3 

    return $ C0If (C0Unbox F0BoolType test) trueBranch falseBranch 

  F0Let (F0Value name _ e) letBody -> do 
    -- Could check if the value is a function before inserting itself into the environment
    value <- codegenExpr ((name, C0RecursiveReference) : env) e 
    letE <- codegenExpr ((name, C0ScopeReference name) : env) letBody 

    return $ C0Declare name value letE 

  F0Lambda argName _ e -> do 
    let captured = Set.toList $ freeVariables e 
        (newEnv, definedClosure) = resolveVars 0 [] [] captured 

    newEnv <- return $ (argName, C0ArgumentReference):newEnv
    translatedExp <- codegenExpr newEnv e 
    functionIndex <- addFunction translatedExp 

    return $ C0MakeClosure functionIndex definedClosure 

    where -- | Returns the new function's environment, and also how to construct the new closure.
          -- *After this function runs, C0ArgumentReference should be added to the newFunctionEnv*
          resolveVars :: Int 
                      -> C0Environment -> [(Symbol, C0VariableReference, Int)] 
                      -> [Symbol]
                      -> (C0Environment, [(Symbol, C0VariableReference, Int)])
          resolveVars closureIndex newFunctionEnv definedClosure = \case 
            [] -> (newFunctionEnv, definedClosure)
            x : xs | x == argName -> resolveVars closureIndex newFunctionEnv definedClosure xs 
                   | otherwise    -> 
                        case lookup x env of 
                          -- If it is not in the environment
                          -- Then it must be bound later in this expression
                          -- This should be unreachable but isn't for some reason 
                          Nothing -> resolveVars closureIndex newFunctionEnv definedClosure xs 
                          Just ref -> 
                            resolveVars (closureIndex + 1) 
                                        ((x, C0ClosureReference closureIndex) : newFunctionEnv)
                                        ((x, ref, closureIndex) : definedClosure)
                                        xs 

programToExpression :: [F0Declaration Symbol Identity] -> F0Expression Symbol Identity
programToExpression decls = foldr F0Let mainExpr (init decls) 
  where F0Value _ _ mainExpr = last decls

forceLookup :: (Show a, Show b, Eq a) => a -> [(a, b)] -> b
forceLookup x e = fromMaybe (error $ "codegen: Variable not found! " ++ show x) $ lookup x e 