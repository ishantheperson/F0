{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Turns an AST where the symbol
-- type is a string into one where
-- the symbol type is able to 
-- distinguish between shadowed variables
module Codegen.Symbolize (symbolize, SymbolErrorType(..), Symbol(..), SymbolError(..)) where 

import Parser.AST 

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map 

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

-- | A symbol consists of a integer
-- which uniquely identifies this variable
-- and a string which represents the name the user gave
newtype Symbol = Symbol (Int, String) deriving Show

instance Eq Symbol where 
  Symbol (a, _) == Symbol (b, _) = a == b 

instance Ord Symbol where 
  compare (Symbol (a, _)) (Symbol (b, _)) = compare a b

type SymbolMap = Map String Symbol 
data SymbolErrorType = UnboundVariable String deriving (Show, Eq)
newtype SymbolError = SymbolError (Maybe SourceRange, SymbolErrorType) deriving (Show, Eq)
type SymbolContext m = (MonadState Int m, MonadWriter [SymbolError] m) 
type Symbolizer m (a :: * -> (* -> *) -> *) (b :: * -> *) = 
  SymbolContext m => SymbolMap -> Maybe SourceRange -> a String b -> m (a Symbol b)

-- | Creates a symbol and updates the new symbol number
mkSymbol :: SymbolContext m => String -> m Symbol 
mkSymbol s = do 
  i <- get 
  modify succ 
  return $ Symbol (i, s) 

-- | This is used to indicate a variable was not found in the context 
dummySymbol :: Symbol 
dummySymbol = Symbol (-1, "")

symbolize :: [F0Declaration String typeInfo] -> Either [SymbolError] [F0Declaration Symbol typeInfo]
symbolize decls = 
  case runWriter $ evalStateT (go Map.empty decls) 0 of 
    (symbolizedDecls, []) -> Right symbolizedDecls
    (_, errors) -> Left errors 

  where go :: SymbolContext m => Map String Symbol -> [F0Declaration String b] -> m [F0Declaration Symbol b]
        go _ [] = return []
        go symbolMap (d:ds)= do 
          (decl, name, symbol) <- symbolizeDecl symbolMap Nothing d
          others <- go (Map.insert name symbol symbolMap) ds 
          return $ decl:others

symbolizeDecl :: SymbolContext m => SymbolMap -> Maybe SourceRange -> F0Declaration String b -> m (F0Declaration Symbol b, String, Symbol)
symbolizeDecl symbolMap position = \case 
  F0DeclPos start decl end -> do 
    (decl, name, symbol) <- symbolizeDecl symbolMap (Just (start, end)) decl
    return (F0DeclPos start decl end, name, symbol)

  F0Value name t body -> do 
    symbol <- mkSymbol name 
    decl <- F0Value symbol t <$> symbolizeExpr symbolMap position body
    return (decl, name, symbol)

  F0Fun name args t body -> do 
    nameSymbol <- mkSymbol name 

    -- Arguments appearing later will shadow arguments appearing earlier 
    -- if they have the same name 
    let (argNames, argTypes) = unzip args 
    argSymbols <- forM argNames mkSymbol
    symbolMap <- return $ Map.fromList (zip argNames argSymbols) <> Map.insert name nameSymbol symbolMap
    decl <- F0Fun nameSymbol (zip argSymbols argTypes) t <$> symbolizeExpr symbolMap position body
    return (decl, name, nameSymbol)


symbolizeExpr :: Symbolizer m F0Expression typeInfo
symbolizeExpr symbolMap position = \case
  F0ExpPos start e end -> F0ExpPos start <$> symbolizeExpr symbolMap (Just (start, end)) e <*> pure end 
  F0Lambda name t e -> do 
    nameSymbol <- mkSymbol name 
    F0Lambda nameSymbol t <$> symbolizeExpr (Map.insert name nameSymbol symbolMap) position e

  F0App e1 e2 -> F0App <$> symbolizeExpr symbolMap position e1 <*> symbolizeExpr symbolMap position e2
  F0OpExp op exps -> F0OpExp op <$> mapM (symbolizeExpr symbolMap position) exps
  F0IntLiteral i -> return $ F0IntLiteral i 
  F0StringLiteral s -> return $ F0StringLiteral s
  F0TypeAssertion e t -> F0TypeAssertion <$> symbolizeExpr symbolMap position e <*> pure t
  F0Identifier name -> 
    case Map.lookup name symbolMap of 
      Just symbol -> return $ F0Identifier symbol 
      Nothing -> do 
        tell [SymbolError (position, UnboundVariable name)]
        return $ F0Identifier dummySymbol

  _ -> error "symbolizeExpr: not yet implemented!"
