import Test.Hspec

import Parser.AST 
import Parser.ASTUtil
import Parser.Internal 

import Codegen.Symbolize

import Text.Megaparsec

import Data.Either (isLeft, fromRight)
import qualified Data.Map.Strict as Map 

tryParse p s = removePositionInfo <$> runParser p "" s

parseExp = tryParse f0Expression
parseDecl = tryParse f0Decl 

-- | Used when you know a string is going to parse into a list of decls 
forceDecls :: String -> [F0Declaration String Maybe]
forceDecls s = map removePositionInfo $ fromRight undefined (runParser f0Decls "" s)

parseExpTests, parseDeclTests :: SpecWith ()
parseExpTests = describe "Expression parsing" $ do 
  it "parses basic identifier" $ 
    parseExp "hello" `shouldBe` Right (F0Identifier "hello")

  it "parses basic arithmetic precedence" $ 
    parseExp "2 + 3 * 4" `shouldBe` Right (F0OpExp Plus [F0IntLiteral 2, 
                                                         F0OpExp Times [F0IntLiteral 3, F0IntLiteral 4]])

parseDeclTests = describe "Decl parsing" $ do 
  it "parses val binding to int literal without type annotation" $
    parseDecl "val x = 3" `shouldBe` Right (F0Value "x" Nothing (F0IntLiteral 3))

  it "parses val binding to int literal with type annotation" $ 
    parseDecl "val x : int = 3" `shouldBe` Right (F0Value "x" (Just F0Int) (F0IntLiteral 3))

  it "does not parse fun decl without arguments" $ 
    parseDecl "fun foo = 3" `shouldSatisfy` isLeft

  it "parses line comments and whitespace correctly" $ 
    parseDecl "fun foo x = -- Line comment\n  x + x" 
      `shouldBe` Right (F0Fun "foo" [("x", Nothing)] Nothing (F0OpExp Plus [F0Identifier "x", F0Identifier "x"]))

symbolizerTests = describe "Symbol conversion tests" $ do 
  it "reports an error for an unbound variable" $
    symbolize (forceDecls "fun foo x = y") `shouldBe` Left [(Nothing, UnboundVariable "y")]

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

main :: IO ()
main = hspec $ do 
  describe "Parser" $ do 
    parseExpTests
    parseDeclTests 

  describe "Elaboration" $ do 
    symbolizerTests
