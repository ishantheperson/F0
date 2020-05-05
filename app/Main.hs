module Main where

import Text.Megaparsec
import Parser.Internal 
import Text.Show.Pretty 
import Control.Monad.Writer.Strict 

run p s = case runParser p "" s of 
  Right r -> pPrint r  
  Left e -> putStrLn (errorBundlePretty e) 

main :: IO ()
main = undefined 
