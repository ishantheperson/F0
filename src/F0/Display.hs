module F0.Display where

class Display a where 
  display :: a -> String 
  displayIO :: a -> IO String 
  displayIO = pure . display
