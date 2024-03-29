import Ast
import Error (Error (Unexpected))
import Lexer
import Parser (program)
import ParserBase
import Test.Hspec
import Token
import Data.Either
import Interpreter

main :: IO ()
main = hspec $ do
  describe "lexer" $ do
    context "when given a valid identifier" $ do
      it "returns an identifier token" $ do
        lexString "a" `shouldBe` Right [IdentifierToken "a"]
    context "when given an invalid character" $ do
      it "returns an unexpected token error" $ do
        lexString "%" `shouldBe` Left [Unexpected "%"]
    context "when given a valid string" $ do
      it "returns the corresponding lex" $ do
        lexString "var a = 5;" `shouldBe` Right [ReservedToken VAR, IdentifierToken "a", ReservedToken EQUAL, NumberToken 5.0, ReservedToken SEMICOLON]
  describe "parser" $ do
    context "when given a valid assignment lex" $ do
      it "returns an AST containing the assignment" $ do
        runParser program [ReservedToken VAR, IdentifierToken "a", ReservedToken EQUAL, NumberToken 5.0, ReservedToken SEMICOLON] `shouldBe` Right (Program [VarDecl (Just (Terminal (NumberToken 5.0))) "a"],[])
    it "succeeds at parsing three additions" $ do
        runParser program [NumberToken 5.0, ReservedToken PLUS, NumberToken 5.0, ReservedToken PLUS, NumberToken 5.0, ReservedToken SEMICOLON] `shouldSatisfy` isRight
    it "allows reassignment of variables" $ do
        runParser program [ReservedToken VAR, IdentifierToken "a", ReservedToken EQUAL, NumberToken 5.0, ReservedToken SEMICOLON, ReservedToken VAR, IdentifierToken "a", ReservedToken EQUAL, NumberToken 5.0, ReservedToken SEMICOLON] `shouldSatisfy` isRight
  describe "interpreter" $ do
    context "when given a valid AST" $ do
      it "evaluates it" $ do
        evalProgram (Program [VarDecl (Just (Terminal (NumberToken 5.0))) "a"]) `shouldSatisfy` isRight
    context "when given an AST containing a reassignment" $ do
      it "evaluates it" $ do
        evalProgram (Program [VarDecl (Just (Terminal (NumberToken 5.0))) "a", VarDecl (Just (Terminal (NumberToken 5.0))) "a"]) `shouldSatisfy` isRight
    context "when given an AST that uses a variable before assigning it" $ do
      it "fails" $ do
        evalProgram (Program [StmtDecl (PrintStmt (Terminal (IdentifierToken "a"))), VarDecl (Just (Terminal (NumberToken 5.0))) "a"]) `shouldSatisfy` isLeft
