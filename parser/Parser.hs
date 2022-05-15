{-# LANGUAGE LambdaCase #-}

module Parser where

import Ast
import Control.Applicative (Alternative (empty, some, (<|>)))
import Error
import ParserBase
import Token

stringFromIdToken :: Parser [Token] String
stringFromIdToken = Parser $ \case
  [] -> Left [EndOfInput]
  ((IdentifierToken s) : rest) -> Right (s, rest)
  x -> Left [Unexpected x]

matchRes :: ReservedTokenType -> Parser [Token] Token
matchRes tt = satisfy $ isReservedTokenOfType tt

matchRes' :: [ReservedTokenType] -> Parser [Token] Token
matchRes' = foldr ((<|>) . matchRes) empty

binaryOpType :: Token -> Parser [Token] BinaryOpType
binaryOpType (ReservedToken BANG_EQUAL) = pure COMP_NOT_EQ
binaryOpType (ReservedToken EQUAL_EQUAL) = pure COMP_EQ
binaryOpType (ReservedToken GREATER_EQUAL) = pure COMP_EQ_OR_GT
binaryOpType (ReservedToken GREATER) = pure COMP_GT
binaryOpType (ReservedToken LESS_EQUAL) = pure COMP_EQ_OR_LT
binaryOpType (ReservedToken LESS) = pure COMP_LT
binaryOpType (ReservedToken MINUS) = pure SUB
binaryOpType (ReservedToken PLUS) = pure ADD
binaryOpType (ReservedToken SLASH) = pure DIV
binaryOpType (ReservedToken STAR) = pure MULT
binaryOpType x = Parser $ \_ -> Left [Unexpected [x]]

unaryOpType :: Token -> Parser [Token] UnaryOpType
unaryOpType (ReservedToken MINUS) = pure NEGATIVE
unaryOpType (ReservedToken BANG) = pure NOT
unaryOpType x = Parser $ \_ -> Left [Unexpected [x]]

binaryOp :: [ReservedTokenType] -> Parser [Token] Expr -> Parser [Token] Expr
binaryOp tt p =
  do
    left <- p
    op <- matchRes' tt >>= binaryOpType
    Binary op left <$> p
    <|> p

unaryOp :: [ReservedTokenType] -> Parser [Token] Expr -> Parser [Token] Expr
unaryOp tt p =
  do
    op <- matchRes' tt >>= unaryOpType
    Unary op <$> p
    <|> p

-- expression -> equality;
-- equality   -> (equality ( "!=" | "==" ) comparison) | comparison ;
-- comparison -> comparison ( ">" | ">=" | "<" | "<=" ) term | term ;
-- term       -> term ( "-" | "+" ) factor | factor;
-- factor     -> factor ( "/" | "*" ) unary | unary ;
-- unary      -> ( ( "!" | "-" ) unary ) | primary ;
-- primary    -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

expression :: Parser [Token] Expr
expression = equality

equality :: Parser [Token] Expr
equality = binaryOp [BANG_EQUAL, EQUAL_EQUAL] comparison

comparison :: Parser [Token] Expr
comparison = binaryOp [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] term

term :: Parser [Token] Expr
term = binaryOp [MINUS, PLUS] factor

factor :: Parser [Token] Expr
factor = binaryOp [SLASH, STAR] unary

unary :: Parser [Token] Expr
unary = unaryOp [BANG, MINUS] primary

primary :: Parser [Token] Expr
primary =
  Terminal <$> (satisfy' [isNumberToken, isStringToken, isIdToken] <|> matchRes' [TRUE, NIL])
    <|> grouping

grouping :: Parser [Token] Expr
grouping = Grouping <$> (matchRes LEFT_PAREN *> expression <* matchRes RIGHT_PAREN)

statement :: Parser [Token] Stmt
statement = ExprStmt <$> expression <|> PrintStmt <$> (matchRes PRINT *> expression)

declaration :: Parser [Token] Decl
declaration =
  ( StmtDecl
      <$> statement
      <|> do
        s <- matchRes VAR *> stringFromIdToken <* matchRes EQUAL
        expr <- expression
        return $ VarDecl (Just expr) s
      <|> VarDecl Nothing
      <$> (matchRes VAR *> stringFromIdToken)
  )
    <* matchRes SEMICOLON

program :: Parser [Token] Program
program = Program <$> some declaration