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
import Parser.ASTUtil
import LibraryBindings

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map 

import Control.Applicative ((<|>))
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

-- | A symbol consists of a integer
-- which uniquely identifies this variable
-- and a string which represents the name the user gave
data Symbol = 
    Symbol (Int, String) 
  | NativeFunction String 
  deriving Show

instance Eq Symbol where 
  Symbol (a, _) == Symbol (b, _) = a == b 
  NativeFunction a == NativeFunction b = a == b 
  _ == _ = False 

instance Ord Symbol where 
  compare (Symbol (a, _)) (Symbol (b, _)) = compare a b
  compare (NativeFunction a) (NativeFunction b) = compare a b 
  compare (NativeFunction _) (Symbol _) = LT 
  compare (Symbol _) (NativeFunction _) = GT 

instance Display Symbol where 
  display (Symbol (_, x)) = x
  display (NativeFunction x) = x 

type SymbolMap = Map String Symbol 
data SymbolErrorType = UnboundVariable String deriving (Show, Eq)
newtype SymbolError = SymbolError (Maybe SourceRange, SymbolErrorType) deriving (Show, Eq)
type SymbolContext m = (MonadState Int m, MonadWriter [SymbolError] m) 
type Symbolizer m (a :: * -> (* -> *) -> *) (b :: * -> *) = 
  SymbolContext m => SymbolMap -> Maybe SourceRange -> a String b -> m (a Symbol b)

instance Display SymbolError where 
  display (SymbolError (range, UnboundVariable x)) = 
    printSourceRange range ++ ": unbound variable '" ++ x ++ "'"

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
          (decl, mappings) <- symbolizeDecl symbolMap Nothing d
          others <- go (Map.fromList mappings `Map.union` symbolMap) ds 
          return $ decl:others

symbolizeDecl :: SymbolContext m => SymbolMap -> Maybe SourceRange -> F0Declaration String b -> m (F0Declaration Symbol b, [(String, Symbol)])
symbolizeDecl symbolMap position = \case 
  F0DeclPos start decl end -> do 
    (decl, mappings) <- symbolizeDecl symbolMap (Just (start, end)) decl
    return (F0DeclPos start decl end, mappings)

  F0Value name t body -> do 
    symbol <- mkSymbol name 
    decl <- F0Value symbol t <$> symbolizeExpr symbolMap position body
    return (decl, [(name, symbol)])

  F0Fun name args t body -> do 
    nameSymbol <- mkSymbol name 

    -- Arguments appearing later will shadow arguments appearing earlier 
    -- if they have the same name 
    let (argNames, argTypes) = unzip args 
    argSymbols <- forM argNames mkSymbol
    symbolMap <- return $ Map.fromList (zip argNames argSymbols) <> Map.insert name nameSymbol symbolMap
    decl <- F0Fun nameSymbol (zip argSymbols argTypes) t <$> symbolizeExpr symbolMap position body
    return (decl, [(name, nameSymbol)])

  F0Data tvs name rules -> do 
    -- Generate a symbol for every constructor 
    newRules <- forM rules $ \(constructor, t) -> do 
                  constructorSym <- mkSymbol constructor 
                  return (constructorSym, t)

    let mapping = zip (map fst rules) (map fst newRules)
    return (F0Data tvs name newRules, mapping)

symbolizeExpr :: Symbolizer m F0Expression typeInfo
symbolizeExpr symbolMap position = \case
  F0ExpPos start e end -> F0ExpPos start <$> symbolizeExpr symbolMap (Just (start, end)) e <*> pure end 
  F0Lambda name t e -> do 
    nameSymbol <- mkSymbol name 
    F0Lambda nameSymbol t <$> symbolizeExpr (Map.insert name nameSymbol symbolMap) position e

  F0App e1 e2 -> F0App <$> symbolizeExpr symbolMap position e1 <*> symbolizeExpr symbolMap position e2
  F0OpExp op exps -> F0OpExp op <$> mapM (symbolizeExpr symbolMap position) exps
  F0Literal l -> return $ F0Literal l
  F0TypeAssertion e t -> F0TypeAssertion <$> symbolizeExpr symbolMap position e <*> pure t
  F0Let decl e -> do 
    (decl, mapping) <- symbolizeDecl symbolMap position decl 
    e <- symbolizeExpr (Map.fromList mapping `Map.union` symbolMap) position e 
    return $ F0Let decl e 

  F0If e1 e2 e3 -> do 
    ~[e1, e2, e3] <- mapM (symbolizeExpr symbolMap position) [e1, e2, e3]
    return $ F0If e1 e2 e3 

  F0Tuple es -> do 
    es <- mapM (symbolizeExpr symbolMap position) es 
    return $ F0Tuple es 

  F0TupleAccess i n e -> do 
    e <- symbolizeExpr symbolMap position e 
    return $ F0TupleAccess i n e 

  F0Identifier name -> F0Identifier <$> replaceName name 
  F0Case obj rules -> do 
    obj <- symbolizeExpr symbolMap position obj 
    rules <- forM rules $ \(constructor, (x, e)) -> do 
               constructor <- replaceName constructor
               xSym <- mkSymbol x
               e <- symbolizeExpr (Map.insert x xSym symbolMap) position e 
               return (constructor, (xSym, e))

    return $ F0Case obj rules 
    
  F0TagValue {} -> error "symbolizeExpr: should not encounter F0TagValue in symbolization phase"

  where replaceName :: SymbolContext m => String -> m Symbol 
        replaceName name = 
          case Map.lookup name symbolMap <|> (NativeFunction name <$ Map.lookup name libraryBindings) of 
            Just symbol -> return symbol 
            Nothing -> do 
              tell [SymbolError (position, UnboundVariable name)]
              return dummySymbol
