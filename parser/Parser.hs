{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative (empty, many, (<|>), some))
import Control.Monad
import ParserBase
import Token
import Ast
import Error

stringFromIdToken :: Parser Token String
stringFromIdToken = Parser $ \case
  [] -> Left [EndOfInput]
  ((IdentifierToken _ s) : rest) -> Right (s, rest)
  (x:xs) -> Left [Unexpected x]

matchRes :: ReservedTokenType -> Parser Token Token
matchRes tt = satisfy $ isReservedTokenOfType tt

matchRes' :: [ReservedTokenType] -> Parser Token Token
matchRes' = foldr ((<|>) . matchRes) empty

binaryOp :: [ReservedTokenType] -> Parser Token Expr -> Parser Token Expr
binaryOp ts p =
  do
    l <- p
    b <- Binary <$> matchRes' ts
    r <- binaryOp ts p <|> p
    return $ b l r
    <|> p

unaryOp :: [ReservedTokenType] -> Parser Token Expr -> Parser Token Expr
unaryOp ts p = (Unary <$> matchRes' ts <*> unaryOp ts p) <|> p

groupingOp :: Parser Token Expr
groupingOp = matchRes LEFT_PAREN *> (Grouping <$> expression) <* matchRes RIGHT_PAREN

-- expression -> equality;
-- equality   -> (equality ( "!=" | "==" ) comparison) | comparison ;
-- comparison -> comparison ( ">" | ">=" | "<" | "<=" ) term | term ;
-- term       -> term ( "-" | "+" ) factor | factor;
-- factor     -> factor ( "/" | "*" ) unary | unary ;
-- unary      -> ( ( "!" | "-" ) unary ) | primary ;
-- primary    -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

expression :: Parser Token Expr
expression =
  binaryOp [BANG_EQUAL, EQUAL_EQUAL] $ -- equality
    binaryOp [LESS, LESS_EQUAL, GREATER, GREATER_EQUAL] $ -- comparison
      binaryOp [MINUS, PLUS] $ -- term
        binaryOp [SLASH, STAR] $ -- factor
          unaryOp [MINUS, BANG] $ -- unary
            Terminal <$> matchRes' [TRUE, FALSE, NIL] <|> groupingOp -- primary
            -- NUMBER, STRING, 

statement :: Parser Token Stmt
statement = (ExprStmt <$> expression <|> PrintStmt <$> matchRes PRINT <*> expression) <* matchRes SEMICOLON

declaration :: Parser Token Decl
declaration = StmtDecl <$> statement
  <|> do
    s <- matchRes VAR *> stringFromIdToken <* matchRes EQUAL
    expr <- expression
    return $ VarDecl (Just expr) s
  <|> VarDecl Nothing <$> (matchRes VAR *> stringFromIdToken)

program :: Parser Token Program
program = Program <$> many declaration <* matchRes EOF