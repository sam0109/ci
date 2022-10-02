{-# LANGUAGE TupleSections #-}
module Interpreter where

import Ast
import Error
import State
import Token

evalExpr :: State -> Expr -> Either [Error Value] (Value, State)
evalExpr s (Terminal (StringToken st)) = Right (Text st, s)
evalExpr s (Terminal (NumberToken n)) = Right (Number n, s)
evalExpr s (Terminal (IdentifierToken name)) = (, s) <$> getVar s name
evalExpr s (Terminal (ReservedToken t)) = case t of
  TRUE -> Right (Boolean True, s)
  FALSE -> Right (Boolean False, s)
  NIL -> Right (Nil, s)
  _ -> Left [UnsupportedValue Nil]
evalExpr s (Unary op expr) = case op of
  NEGATIVE -> (, s) . Number . negate <$> (evalExpr s expr >>= extractNumber . fst)
  NOT -> (, s) . Boolean . not <$> (evalExpr s expr >>= extractBoolean . fst)
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
    evalNumOp f le re = (, s) . Number <$> (f <$> (evalExpr s le >>= extractNumber . fst) <*> (evalExpr s re >>= extractNumber . fst))
    evalTextOp f le re = (, s) . Text <$> (f <$> (evalExpr s le >>= extractText . fst) <*> (evalExpr s re >>= extractText . fst))
    evalComp f le re = (, s) . Boolean <$> (f <$> (fst <$> evalExpr s le) <*> (fst <$> evalExpr s re))
evalExpr s (Grouping a) = evalExpr s a
evalExpr s (Assign name expr) = do
  (expr_val, expr_state) <- evalExpr s expr
  return (expr_val, setVar expr_state (Var name expr_val))

evalStmt :: State -> Stmt -> Either [Error Value] State
evalStmt s (ExprStmt a) = snd <$> evalExpr s a
evalStmt s (PrintStmt a) = evalExpr s a >>= (Right . performIO s . print . fst)

evalDecl :: State -> Decl -> Either [Error Value] State
evalDecl s (StmtDecl stmt) = evalStmt s stmt
evalDecl s (VarDecl Nothing name) = Right $ setVar s (Var name Nil)
evalDecl s (VarDecl (Just expr) name) = setVar s . Var name . fst <$> evalExpr s expr 

evalProgram' :: State -> Program -> Either [Error Value] State
evalProgram' s (Program []) = Right s
evalProgram' s (Program (x : xs)) = evalDecl s x >>= flip evalProgram' (Program xs)

evalProgram :: Program -> Either [Error Value] State
evalProgram = evalProgram' (State (return ()) [])