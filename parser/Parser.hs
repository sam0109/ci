{-# LANGUAGE LambdaCase #-}

module Parser where

import Ast
import Control.Applicative (Alternative (empty, (<|>), some))
import Error
import ParserBase
import Token

stringFromIdToken :: Parser Token String
stringFromIdToken = Parser $ \case
  [] -> Left [EndOfInput]
  ((IdentifierToken s) : rest) -> Right (s, rest)
  (x : _) -> Left [Unexpected x]

matchRes :: ReservedTokenType -> Parser Token Token
matchRes tt = satisfy $ isReservedTokenOfType tt

matchRes' :: [ReservedTokenType] -> Parser Token Token
matchRes' = foldr ((<|>) . matchRes) empty

binaryOp :: [ReservedTokenType] -> Parser Token Expr -> Parser Token Expr
binaryOp tt p = do
    left <- p
    op <- matchRes' tt
    Binary op left <$> p
    <|> p

unaryOp :: [ReservedTokenType] -> Parser Token Expr -> Parser Token Expr
unaryOp tt p = do
    op <- matchRes' tt
    Unary op <$> p
    <|> p

-- expression -> equality;
-- equality   -> (equality ( "!=" | "==" ) comparison) | comparison ;
-- comparison -> comparison ( ">" | ">=" | "<" | "<=" ) term | term ;
-- term       -> term ( "-" | "+" ) factor | factor;
-- factor     -> factor ( "/" | "*" ) unary | unary ;
-- unary      -> ( ( "!" | "-" ) unary ) | primary ;
-- primary    -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

expression :: Parser Token Expr
expression = equality

equality :: Parser Token Expr
equality = binaryOp [BANG_EQUAL, EQUAL_EQUAL] comparison

comparison :: Parser Token Expr
comparison = binaryOp [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] term

term :: Parser Token Expr
term = binaryOp [MINUS, PLUS] factor

factor :: Parser Token Expr
factor = binaryOp [SLASH, STAR] unary

unary :: Parser Token Expr
unary = unaryOp [BANG, MINUS] primary

primary :: Parser Token Expr
primary =
  Terminal <$> (satisfy isNumberToken <|> satisfy isStringToken <|> matchRes' [TRUE, NIL])
    <|> grouping

grouping :: Parser Token Expr
grouping = Grouping <$>  (matchRes LEFT_PAREN *> expression <* matchRes RIGHT_PAREN)

statement :: Parser Token Stmt
statement = (ExprStmt <$> expression <|> PrintStmt <$> matchRes PRINT <*> expression) <* matchRes SEMICOLON

declaration :: Parser Token Decl
declaration =
  StmtDecl <$> statement
    <|> do
      s <- matchRes VAR *> stringFromIdToken <* matchRes EQUAL
      expr <- expression
      return $ VarDecl (Just expr) s
    <|> VarDecl Nothing <$> (matchRes VAR *> stringFromIdToken)

program :: Parser Token Program
program = Program <$> some declaration