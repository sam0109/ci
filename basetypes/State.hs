module State where

import Error

data Value
  = Number Float
  | Text String
  | Boolean Bool
  | Nil
  deriving (Eq)

instance Show Value where
  show Nil = "Nil"
  show (Number f) = show f
  show (Text t) = show t
  show (Boolean b) = show b

data Var = Var
  { identifier :: String,
    val :: Value
  }

data State = State
  { io :: IO (),
    vars :: [Var]
  }

performIO :: State -> IO () -> State
performIO s i = State (i >> io s) (vars s)

createVar :: State -> Var -> State
createVar s v = State (io s) (v : vars s)

findVar :: State -> String -> Maybe Var
findVar (State _ []) str = Nothing
findVar (State i (x : xs)) str =
  if identifier x == str
    then Just x
    else findVar (State i xs) str

removeVar :: [Var] -> String -> [Var]
removeVar [] s = []
removeVar (x : xs) s =
  if identifier x == s
    then xs
    else x : removeVar xs s

setVar :: State -> Var -> State
setVar s v = case findVar s (identifier v) of
  Nothing -> State (io s) (v : vars s)
  Just _ -> State (io s) (v : removeVar (vars s) (identifier v))

declVar :: State -> String -> Either [Error String] State
declVar s id = case findVar s id of
  Nothing -> Right $ State (io s) (Var id Nil : vars s)
  Just _ -> Left [VariableAlreadyDeclared id]
