module Ast where

import Error
import Token

renderAst :: (Show i, Show a) => Either [Error i] (a, [i]) -> String
renderAst x = case x of
  Left err -> show err
  Right (ast, _) -> show ast

data UnaryOpType
  = NEGATIVE
  | NOT
  deriving (Show, Enum, Eq)

data BinaryOpType
  = COMP_NOT_EQ
  | COMP_EQ
  | COMP_EQ_OR_GT
  | COMP_GT
  | COMP_EQ_OR_LT
  | COMP_LT
  | SUB
  | ADD
  | DIV
  | MULT
  deriving (Show, Enum, Eq)

data Expr
  = Terminal Token
  | Unary UnaryOpType Expr
  | Binary BinaryOpType Expr Expr
  | Assign String Expr
  | Grouping Expr
  deriving (Show, Eq)

data Stmt
  = ExprStmt Expr
  | PrintStmt Expr
  deriving (Show, Eq)

data Decl
  = VarDecl (Maybe Expr) String
  | StmtDecl Stmt
  deriving (Show, Eq)

data Program = Program [Decl]
  deriving (Show, Eq)
