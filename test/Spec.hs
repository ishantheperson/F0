import Test.Hspec

import Parser.AST 
import Parser.ASTUtil
import Parser.Internal 

import Codegen.Symbolize

import Typechecker.Infer

import Text.Megaparsec

import Data.Either (isLeft, fromRight)
import qualified Data.Map.Strict as Map 
import qualified Data.Set as Set 

tryParse p s = removePositionInfo <$> runParser p "" s

parseExp = tryParse f0Expression
parseType = runParser f0Type ""
parseDecl = tryParse f0Decl 

typecheckE :: String -> Either [TypeError] F0Type
typecheckE s = 
  let (F0Value _ _ e) = head $ fromRight undefined $ symbolize $ forceDecls ("val foo = " ++ s)
  in snd <$> typecheck e 

-- | Used when you know a string is going to parse into a list of decls 
forceDecls :: String -> [F0Declaration String Maybe]
forceDecls s = map removePositionInfo $ fromRight undefined (runParser f0Decls "" s)

parseExpTests, parseTypeTests, parseDeclTests :: SpecWith ()
parseExpTests = describe "Expression parsing" $ do 
  it "parses basic identifier" $ 
    parseExp "hello" `shouldBe` Right (F0Identifier "hello")

  it "parses basic arithmetic precedence" $ 
    parseExp "2 + 3 * 4" `shouldBe` Right (F0OpExp Plus [F0IntLiteral 2, 
                                                         F0OpExp Times [F0IntLiteral 3, F0IntLiteral 4]])

parseTypeTests = describe "Type parsing" $ do 
  it "parses function arrows with right associativity" $ 
    parseType "int -> int -> int" `shouldBe` Right (F0Function (F0PrimitiveType F0IntType) (F0Function (F0PrimitiveType F0IntType) (F0PrimitiveType F0IntType)))

  it "parses basic type variables" $ 
    parseType "'a" `shouldBe` Right (F0TypeVariable "a")

  it "parses type variables in functions" $ 
    parseType "'a -> 'b" `shouldBe` Right (F0Function (F0TypeVariable "a") (F0TypeVariable "b"))

parseDeclTests = describe "Declaration parsing" $ do 
  it "parses val binding to int literal without type annotation" $
    parseDecl "val x = 3" `shouldBe` Right (F0Value "x" Nothing (F0IntLiteral 3))

  it "parses val binding to int literal with type annotation" $ 
    parseDecl "val x : int = 3" `shouldBe` Right (F0Value "x" (Just (F0PrimitiveType F0IntType)) (F0IntLiteral 3))

  it "does not parse fun decl without arguments" $ 
    parseDecl "fun foo = 3" `shouldSatisfy` isLeft

  it "parses line comments and whitespace correctly" $ 
    parseDecl "fun foo x = -- Line comment\n  x + x" 
      `shouldBe` Right (F0Fun "foo" [("x", Nothing)] Nothing (F0OpExp Plus [F0Identifier "x", F0Identifier "x"]))

freeVarsTests :: SpecWith ()
freeVarsTests = describe "Free vars of expressions" $ do 
  it "reports a lone identifier as free" $ 
    freeVariables Set.empty (F0Identifier "x") == Set.fromList ["x"]

  it "reports a variable free when it is bound in another lambda" $ 
    freeVariables Set.empty (F0App (F0Lambda "x" Nothing (F0Identifier "x")) (F0Identifier "x")) == Set.fromList ["x"]

  it "reports free variable inside a lambda" $ 
    freeVariables Set.empty (F0App (F0Lambda "x" Nothing (F0Identifier "y")) (F0Identifier "z")) == Set.fromList ["y", "z"]

symbolizerTests :: SpecWith ()
symbolizerTests = describe "Symbol conversion tests" $ do 
  it "reports an error for an unbound variable" $
    symbolize (forceDecls "fun foo x = y") `shouldBe` Left [(Nothing, UnboundVariable "y")]

  it "reports an error for an unbound variable which is bound in another expression" $ 
    symbolize (forceDecls "val f = (fn x => x) x") `shouldBe` Left [(Nothing, UnboundVariable "x")]

  it "properly distinguishes shadowed bindings across decls" $ 
    symbolize (forceDecls "val x = 3\nval x = x") `shouldBe` (Right [
      F0Value (Symbol (0, "x")) Nothing (F0IntLiteral 3),
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
typeInferenceTests = describe "Type inference tests" $ do 
  it "gives the correct type to the identity function" $ 
    typecheckE "fn x => x" `shouldBe` Right (F0TypeVariable "a" `F0Function` F0TypeVariable "a")

  it "rejects an infinite type" $
    typecheckE "fn x => x x" `shouldSatisfy` isLeft 

  it "respects expression type annotation restrict polymorphism" $ 
    typecheckE "fn x => (fn y => x : int) x" `shouldBe` Right (
      F0PrimitiveType F0IntType `F0Function` F0PrimitiveType F0IntType
    )

  it "gives the correct type to the constant function" $ 
    typecheckE "fn x => fn y => x" `shouldBe` Right (
      F0TypeVariable "a" `F0Function` (F0TypeVariable "b" `F0Function` F0TypeVariable "a")
    )

  it "respects type annotation on a lambda" $ 
    typecheckE "fn x => fn y : int => x" `shouldBe` Right (
      F0TypeVariable "a" `F0Function` (F0PrimitiveType F0IntType `F0Function` F0TypeVariable "a")
    )

main :: IO ()
main = hspec $ do 
  describe "Parser" $ do 
    parseExpTests
    parseTypeTests
    parseDeclTests 

  describe "Elaboration" $ do 
    freeVarsTests
    symbolizerTests

  describe "Typechecking/inference" $ do 
    typeInferenceTests 