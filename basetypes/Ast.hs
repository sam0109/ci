module Ast where

import Error
import Token

renderAst :: (Show i, Show a) => Either [Error i] (a, [i]) -> String
renderAst x = case x of
  Left err -> show err
  Right (ast, rest) -> show ast

data Expr
  = Terminal Token
  | Unary Token Expr
  | Binary Token Expr Expr
  | Grouping Expr
  deriving (Show)

data Stmt
  = ExprStmt Expr
  | PrintStmt Token Expr
  deriving (Show)

data Decl
  = VarDecl (Maybe Expr) String
  | StmtDecl Stmt

data Program = Program [Decl]
