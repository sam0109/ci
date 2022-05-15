module Interpreter where

import Ast
import Error
import State
import Token

evalExpr :: State -> Expr -> Either [Error Value] Value
evalExpr _ (Terminal (StringToken st)) = Right $ Text st
evalExpr _ (Terminal (NumberToken n)) = Right $ Number n
evalExpr s (Terminal (IdentifierToken name)) = getVar s name
evalExpr _ (Terminal (ReservedToken t)) = case t of
  TRUE -> Right $ Boolean True
  FALSE -> Right $ Boolean False
  NIL -> Right Nil
  _ -> Left [UnsupportedValue Nil]
evalExpr s (Unary op expr) = case op of
  NEGATIVE -> Number . negate <$> (evalExpr s expr >>= extractNumber)
  NOT -> Boolean . not <$> (evalExpr s expr >>= extractBoolean)
evalExpr s (Binary op l r) = case op of
  COMP_EQ -> evalComp (==) l r
  COMP_NOT_EQ -> evalComp (/=) l r
  COMP_LT -> evalComp (<) l r
  COMP_EQ_OR_LT -> evalComp (<=) l r
  COMP_GT -> evalComp (>) l r
  COMP_EQ_OR_GT -> evalComp (>=) l r
  SUB -> evalNumOp (-) l r
  ADD -> evalNumOp (+) l r <> evalTextOp (++) l r
  DIV -> evalNumOp (/) l r
  MULT -> evalNumOp (*) l r
  where
    evalNumOp f le re = Number <$> (f <$> (evalExpr s le >>= extractNumber) <*> (evalExpr s re >>= extractNumber))
    evalTextOp f le re = Text <$> (f <$> (evalExpr s le >>= extractText) <*> (evalExpr s re >>= extractText))
    evalComp f le re = Boolean <$> (f <$> evalExpr s le <*> evalExpr s re)
evalExpr s (Grouping a) = evalExpr s a

evalStmt :: State -> Stmt -> Either [Error Value] State
evalStmt s (ExprStmt a) = evalExpr s a >> Right s --TODO: deal with state modifications
evalStmt s (PrintStmt a) = evalExpr s a >>= (Right . performIO s . print)

evalDecl :: State -> Decl -> Either [Error Value] State
evalDecl s (StmtDecl stmt) = evalStmt s stmt
evalDecl s (VarDecl Nothing name) = declVar s name Nil
evalDecl s (VarDecl (Just expr) name) = evalExpr s expr >>= declVar s name

evalProgram' :: State -> Program -> Either [Error Value] State
evalProgram' s (Program []) = Right s
evalProgram' s (Program (x : xs)) = evalDecl s x >>= flip evalProgram' (Program xs)

evalProgram :: Program -> Either [Error Value] State
evalProgram = evalProgram' (State (return ()) [])