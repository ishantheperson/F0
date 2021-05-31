{-# LANGUAGE LambdaCase #-}
module Repl (repl) where 

import Data.Functor
import Data.Foldable

import qualified Data.Map.Strict as Map

import System.Console.Haskeline hiding (display)

import F0.Interpret
import F0.Codegen.Symbolize

repl :: IO ()
repl = runInputT defaultSettings (loop emptyContext)

loop :: EvalContext -> InputT IO () 
loop context = do 
  run context >>= \case 
    Nothing -> return () 
    Just newContext -> loop newContext 

run :: EvalContext -> InputT IO (Maybe EvalContext)
run context = do 
  getInputLine "> " >>= \case 
    Nothing -> return () 
    Just ":vars" -> 
      forM_ (Map.toList context) $ \(Symbol (_, name), val) -> 
        outputStrLn $ name ++ ": " ++ show val 

  undefined 