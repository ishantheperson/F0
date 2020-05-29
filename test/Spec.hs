{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
import Test.Hspec

import System.Exit
import System.Directory
import System.Process 
import System.FilePath.Posix

import F0.Parser.AST 
import F0.Parser.ASTUtil
import F0.Parser.Internal 

import F0.Codegen.Symbolize
import F0.Codegen.Closure
import F0.Codegen.PrintC0

import F0.Compiler.Compile 

import F0.Typechecker.Infer

import Text.Megaparsec

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Data.Either (isLeft, fromRight)
import qualified Data.Map.Strict as Map 
import qualified Data.Set as Set 

import F0.Display

tryParse p s = removePositionInfo <$> runParser p "" s

parseExp = tryParse f0Expression
parseType = runParser f0Type ""
parseDecl = tryParse f0Decl 

typecheckE :: String -> Either TypeError F0Type
typecheckE s = 
  let (F0Value _ _ e) = head $ forceSymbols ("val foo = " ++ s)
  in typecheckExpr e 

typecheckD :: String -> Symbol -> Either TypeError Scheme
typecheckD s n = 
  let syms = forceSymbols s 
  in case fst <$> typecheck emptyEnv defaultState syms of 
       Left e -> Left e 
       Right env -> Right $ fromJust $ getSymbolType env n 

-- | Used when you know a string is going to parse into a list of decls 
forceDecls :: String -> [F0Declaration String Maybe]
forceDecls s = map removePositionInfo $ fromRight (error "forceDecls: Parsing failed") (runParser f0Decls "" s)

forceSymbols :: String -> [F0Declaration Symbol Maybe]
forceSymbols s = fromRight (error "typecheckE: Symbolization failed") $ symbolize $ forceDecls s 

moveToTestDir :: IO ()
moveToTestDir = setCurrentDirectory "test/testcases"

-- | Takes a path to a .sml file in the testcases directory.
-- There should also be a .txt file with the expected stdout 
integrateTest inputFile = do 
  text <- readFile inputFile 
  case compile inputFile text of 
    Left e -> fail (display e)
    Right (c1source -> c1src) -> do
      let outputFileName = dropExtension inputFile ++ ".c1"
          expectationFileName = dropExtension inputFile ++ ".txt"

      writeFile outputFileName c1src
      (status, stdout, stderr) <- readProcessWithExitCode "cc0" ["-x", outputFileName] ""
      expected <- readFile expectationFileName

      stdout `shouldBe` expected 

parseExpTests, parseTypeTests, parseDeclTests :: SpecWith ()
parseExpTests = describe "Expression parsing" $ do 
  it "parses basic identifier" $ 
    parseExp "hello" `shouldBe` Right (F0Identifier "hello")

  it "parses basic arithmetic precedence" $ 
    parseExp "2 + 3 * 4" `shouldBe` Right (F0OpExp Plus [f0Int 2, 
                                                         F0OpExp Times [f0Int 3, f0Int 4]])

  it "parses function application with left associativity" $ 
    parseExp "f a b c" `shouldBe` Right (F0App (F0App (F0App (F0Identifier "f") (F0Identifier "a")) (F0Identifier "b")) (F0Identifier "c"))

parseTypeTests = describe "Type parsing" $ do 
  it "parses function arrows with right associativity" $ 
    parseType "int -> int -> int" `shouldBe` Right (F0Function (f0IntT) (F0Function (f0IntT) (f0IntT)))

  it "parses basic type variables" $ 
    parseType "'a" `shouldBe` Right (F0TypeVariable "a")

  it "parses type variables in functions" $ 
    parseType "'a -> 'b" `shouldBe` Right (F0Function (F0TypeVariable "a") (F0TypeVariable "b"))

parseDeclTests = describe "Declaration parsing" $ do 
  it "parses val binding to int literal without type annotation" $
    parseDecl "val x = 3" `shouldBe` Right [F0Value "x" Nothing (f0Int 3)]

  -- it "parses val binding to int literal with type annotation" $ 
  --   parseDecl "val x : int = 3" `shouldBe` Right [F0Value "x" (Just (f0IntT)) (f0Int 3)]

  it "does not parse fun decl without arguments" $ 
    parseDecl "fun foo = 3" `shouldSatisfy` isLeft

  it "parses line comments and whitespace correctly" $ 
    parseDecl "fun foo x = -- Line comment\n  x + x" 
      `shouldBe` Right [F0Fun "foo" [("x", Nothing)] Nothing (F0OpExp Plus [F0Identifier "x", F0Identifier "x"])]

freeVarsTests :: SpecWith ()
freeVarsTests = describe "Free vars of expressions" $ do 
  it "reports a lone identifier as free" $ 
    freeVariables (F0Identifier "x") `shouldBe` Set.fromList ["x"]

  it "reports a variable free when it is bound in another lambda" $ 
    freeVariables (F0App (F0Lambda "x" Nothing (F0Identifier "x")) (F0Identifier "x")) `shouldBe` Set.fromList ["x"]

  it "reports free variable inside a lambda" $ 
    freeVariables (F0App (F0Lambda "x" Nothing (F0Identifier "y")) (F0Identifier "z")) `shouldBe` Set.fromList ["y", "z"]

  it "doesn't report bound variables as free" $ 
    freeVariables (F0Lambda "x" Nothing (F0Identifier "x")) `shouldBe` Set.empty

symbolizerTests :: SpecWith ()
symbolizerTests = describe "Symbol conversion tests" $ do 
  it "reports an error for an unbound variable" $
    symbolize (forceDecls "fun foo x = y") `shouldBe` Left [SymbolError (Nothing, UnboundVariable "y")]

  it "reports an error for an unbound variable which is bound in another expression" $ 
    symbolize (forceDecls "val f = (fn x => x) x") `shouldBe` Left [SymbolError (Nothing, UnboundVariable "x")]

  it "properly distinguishes shadowed bindings across decls" $ 
    symbolize (forceDecls "val x = 3\nval x = x") `shouldBe` (Right [
      F0Value (Symbol (0, "x")) Nothing (f0Int 3),
      F0Value (Symbol (1, "x")) Nothing (F0Identifier (Symbol (0, "x")))
    ])

  it "allows recursive use of a function" $ 
    symbolize (forceDecls "fun foo x = foo x") `shouldBe` (Right [
      F0Fun (Symbol (0, "foo")) [(Symbol (1, "x"), Nothing)] Nothing 
        (F0App (F0Identifier (Symbol (0, "foo"))) (F0Identifier (Symbol (1, "x"))))
    ])

  it "properly distinguishes shadowed bindings in an expression" $ 
    symbolize (forceDecls "fun foo x = fn x => foo x") `shouldBe` (Right [
      F0Fun (Symbol (0, "foo")) [(Symbol (1, "x"), Nothing)] Nothing 
        (F0Lambda (Symbol (2, "x")) Nothing (
            F0App (F0Identifier (Symbol (0, "foo"))) (F0Identifier (Symbol (2, "x")))
        ))
    ])

typeInferenceTests :: SpecWith ()
typeInferenceTests = do 
  it "gives the correct type to the identity function" $ 
    typecheckE "fn x => x" `shouldBe` Right (F0TypeVariable "_x0" `F0Function` F0TypeVariable "_x0")

  it "rejects an infinite type" $
    typecheckE "fn x => x x" `shouldSatisfy` isLeft 

  -- Removed expression type annotations
  -- it "respects expression type annotation restrict polymorphism" $ 
  --   typecheckE "fn x => (fn y => x : int) x" `shouldBe` Right (
  --     f0IntT `F0Function` f0IntT
  --   )

  it "gives the correct type to the constant function" $ 
    typecheckE "fn x => fn y => x" `shouldBe` Right (
      F0TypeVariable "_x0" `F0Function` (F0TypeVariable "_x1" `F0Function` F0TypeVariable "_x0")
    )

  -- it "respects type annotation on a lambda" $ 
  --   typecheckE "fn x => fn y : int => x" `shouldBe` Right (
  --     F0TypeVariable "a" `F0Function` (f0IntT `F0Function` F0TypeVariable "a")
  --   )

  -- it "distinguishes different given type variables" $ 
  --   typecheckE "fn (x: 'a) => (x: 'b)" `shouldSatisfy` isLeft 

  it "can check a polymorphic application inside a lambda" $
    typecheckE "fn x => (fn y => y x) (fn x => x)" `shouldBe` Right (
      F0TypeVariable "_x4" `F0Function` F0TypeVariable "_x4"
    )

  it "checks the looping function" $ 
    typecheckD "fun f x = f x" (Symbol (0, "f")) `shouldBe` Right (Forall ["_x1", "_x2"] $ F0TypeVariable "_x1" `F0Function` F0TypeVariable "_x2")

  it "checks a higher order function" $ 
    typecheckD "fun add mkInt a b = mkInt a + mkInt b" (Symbol (0, "add")) `shouldBe`
      Right (Forall ["_x3"] (F0Function (F0Function (F0TypeVariable "_x3") (f0IntT)) (F0Function (F0TypeVariable "_x3") (F0Function (F0TypeVariable "_x3") (f0IntT)))))

  it "resolves shadowing between arguments and let binding correctly" $ 
    typecheckD "fun foo x = let val x = 3 in x end" (Symbol (0, "foo")) `shouldBe`
      Right (Forall ["_x1"] $ F0TypeVariable "_x1" `F0Function` f0IntT)

  it "resolves shadowing between multiple bindings in a let expression correctly" $ 
    typecheckD "fun foo x = let val x = 3 val x = \"hello\" in x end" (Symbol (0, "foo")) `shouldBe`
      Right (Forall ["_x1"] $ F0TypeVariable "_x1" `F0Function` f0StringT)

  it "gives the correct type to parameters used in an if expression" $ 
    typecheckD "fun foo a b c = if a then b else c" (Symbol (0, "foo")) `shouldBe`
      Right (Forall ["_x3"] $ f0BoolT `F0Function` F0TypeVariable "_x3" `F0Function` (F0TypeVariable "_x3" `F0Function` F0TypeVariable "_x3"))

  it "won't typecheck a function where the arguments have been applied in the wrong order" $ 
    typecheckD "fun loop f n = if n == 0 then () else let val () = f n in loop (n - 1) f end" (Symbol (0, "loop")) `shouldSatisfy` isLeft 
      
  it "typechecks a higher order function with looping and using () as a name" $ 
    typecheckD "fun loop f n = if n == 0 then () else let val () = f n in loop f (n - 1) end" (Symbol (0, "loop")) `shouldBe` 
      Right (Forall ["_x3"] $ (f0IntT `F0Function` F0TypeVariable "_x3") `F0Function` f0IntT `F0Function` f0UnitT)

  it "typechecks the factorial function" $ 
    typecheckD "fun fact n = if n == 0 then 1 else n * fact (n - 1)" (Symbol (0, "fact")) `shouldBe`
      Right (Forall [] $ f0IntT `F0Function` f0IntT)

  it "typechecks the double loop function" $ 
    typecheckD "fun f x = f (f x)" (Symbol (0, "f")) `shouldBe`
      Right (Forall ["_x2"] $ F0TypeVariable "_x2" `F0Function` F0TypeVariable "_x2")

  it "typechecks the curried equality function" $ 
    typecheckD "fun eq a b = a == b" (Symbol (0, "eq")) `shouldBe`
      Right (Forall [] $ f0IntT `F0Function` f0IntT `F0Function` f0BoolT)

  it "typechecks the find function" $ 
    typecheckD "fun find p n = if n == 0 then 0 else if p (4 * n) then n else find p (n - 1)" (Symbol (0, "find")) `shouldBe`
      Right (Forall [] $ (f0IntT `F0Function` f0BoolT) `F0Function` f0IntT `F0Function` f0IntT)

  it "uses constraints of a branch" $ 
    typecheckD "fun foo a b = if true then a else b" (Symbol (0, "foo")) `shouldBe`
      Right (Forall ["_x2"] $ F0TypeVariable "_x2" `F0Function` F0TypeVariable "_x2" `F0Function` F0TypeVariable "_x2")

integrationTests :: SpecWith () 
integrationTests = do 
  it "correctly calculates 10!" $ 
    integrateTest "higher_order_fact.sml" 

  it "correctly calculates Fibonacci numbers" $
    integrateTest "fib.sml"

  it "correctly executes the find testcase" $ 
    integrateTest "find.sml"

  it "correctly executes the uncurred addition testcase" $ 
    integrateTest "tuple_addition.sml"

  it "correctly executes the curry/uncurry function testcase" $ 
    integrateTest "curry_uncurry.sml"

  it "correctly executes the list summation testcase" $ 
    integrateTest "list_sum.sml"

  it "correctly executes the list 'quick'sort testcase" $ 
    integrateTest "sort.sml"

  it "correctly executes the queue testcase" $
    integrateTest "queue.sml"

  it "correctly executed the either monad testcase" $ 
    integrateTest "monad.sml"

main :: IO ()
main = hspec $ do 
  context "Parser" $ do 
    parseExpTests
    parseTypeTests
    parseDeclTests 

  context "Elaboration" $ do 
    freeVarsTests
    symbolizerTests

  context "Typechecking/inference" $ do 
    typeInferenceTests 

  beforeAll_ moveToTestDir $ 
    context "Codegeneration testing via CC0" $ do 
      integrationTests