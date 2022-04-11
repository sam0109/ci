module Interpreter where

import Token
import State
import Error
import Ast

negative :: Token -> Value -> Either [Error Token] Value
negative _ (Number f) = Right $ Number $ negate f
negative t _ = Left [InvalidOperation t]

bang :: Token -> Value -> Either [Error Token] Value
bang _ (Boolean b) = Right $ Boolean $ not b
bang t _ = Left [InvalidOperation t]

lt :: Token -> Value -> Value -> Either [Error Token] Value
lt _ (Number l) (Number r) = Right $ Boolean $ l < r
lt t _ _ = Left [InvalidOperation t]

lte :: Token -> Value -> Value -> Either [Error Token] Value
lte _ (Number l) (Number r) = Right $ Boolean $ l <= r
lte t _ _ = Left [InvalidOperation t]

gt :: Token -> Value -> Value -> Either [Error Token] Value
gt _ (Number l) (Number r) = Right $ Boolean $ l > r
gt t _ _ = Left [InvalidOperation t]

gte :: Token -> Value -> Value -> Either [Error Token] Value
gte _ (Number l) (Number r) = Right $ Boolean $ l >= r
gte t _ _ = Left [InvalidOperation t]

minus :: Token -> Value -> Value -> Either [Error Token] Value
minus _ (Number l) (Number r) = Right $ Number $ l - r
minus t _ _ = Left [InvalidOperation t]

divide :: Token -> Value -> Value -> Either [Error Token] Value
divide _ (Number l) (Number r) = Right $ Number $ l / r
divide t _ _ = Left [InvalidOperation t]

multiply :: Token -> Value -> Value -> Either [Error Token] Value
multiply _ (Number l) (Number r) = Right $ Number $ l * r
multiply t _ _ = Left [InvalidOperation t]

plus :: Token -> Value -> Value -> Either [Error Token] Value
plus _ (Number l) (Number r) = Right $ Number $ l + r
plus _ (Text l) (Text r) = Right $ Text $ l ++ r
plus t _ _ = Left [InvalidOperation t]

evalExpr :: Expr -> Either [Error Token] Value
evalExpr (Terminal tok@(StringToken _ s)) = Right $ Text s
evalExpr (Terminal tok@(NumberToken _ n)) = Right $ Number n
evalExpr (Terminal tok@(IdentifierToken t v)) = Left [UnsupportedValue tok]
evalExpr (Terminal tok@(ReservedToken t v)) = case t of
  TRUE -> Right $ Boolean True
  FALSE -> Right $ Boolean False
  NIL -> Right Nil
  _ -> Left [UnsupportedValue tok]
evalExpr (Unary tok@(ReservedToken t v) a) = case t of
  MINUS -> evalExpr a >>= negative tok
  BANG -> evalExpr a >>= bang tok
  _ -> Left [UnsupportedValue tok]
-- [BANG_EQUAL, EQUAL_EQUAL] -- equality
-- [LESS, LESS_EQUAL, GREATER, GREATER_EQUAL] -- comparison
-- [MINUS, PLUS] -- term
-- [SLASH, STAR] -- factor
evalExpr (Binary tok@(ReservedToken t v) l r) = case t of
  BANG_EQUAL -> Boolean <$> ((==) <$> evalExpr l <*> evalExpr r)
  EQUAL_EQUAL -> Boolean <$> ((/=) <$> evalExpr l <*> evalExpr r)
  LESS -> do
    lval <- evalExpr l
    rval <- evalExpr r
    lt tok lval rval
  LESS_EQUAL -> do
    lval <- evalExpr l
    rval <- evalExpr r
    lte tok lval rval
  GREATER -> do
    lval <- evalExpr l
    rval <- evalExpr r
    gt tok lval rval
  GREATER_EQUAL -> do
    lval <- evalExpr l
    rval <- evalExpr r
    gte tok lval rval
  MINUS -> do
    lval <- evalExpr l
    rval <- evalExpr r
    minus tok lval rval
  PLUS -> do
    lval <- evalExpr l
    rval <- evalExpr r
    plus tok lval rval
  SLASH -> do
    lval <- evalExpr l
    rval <- evalExpr r
    divide tok lval rval
  STAR -> do
    lval <- evalExpr l
    rval <- evalExpr r
    multiply tok lval rval
  _ -> Left [UnsupportedValue tok]
evalExpr (Grouping a) = evalExpr a

evalStmt :: State -> Stmt -> Either [Error Token] State
evalStmt s (ExprStmt a) = evalExpr a >> Right s
evalStmt s (PrintStmt tok@(ReservedToken t v) a) = case t of
  PRINT -> evalExpr a >>= (Right . performIO s . print)
  _ -> Left [UnsupportedValue tok]

evalDecl :: State -> Decl -> Either [Error Token] State
evalDecl s (StmtDecl stmt) = evalStmt s stmt
evalDecl s (VarDecl t e) = case e of
  Nothing -> declVar (value t)
  Just ex -> _

evaluate' :: State -> Program -> Either [Error Token] State
evaluate' s (Program []) = Right s
evaluate' s (Program (x : xs)) = evalStmt s x >>= flip evaluate' (Program xs)

evaluate :: Program -> Either [Error Token] State
evaluate = evaluate' (return ())