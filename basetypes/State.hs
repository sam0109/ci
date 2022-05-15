module State where

import Error

data Value
  = Number Float
  | Text String
  | Boolean Bool
  | Nil
  deriving (Eq, Ord)

extractNumber :: Value -> Either [Error Value] Float
extractNumber (Number n) = Right n
extractNumber v = Left [Unexpected v]

extractText :: Value -> Either [Error Value] String
extractText (Text t) = Right t
extractText v = Left [Unexpected v]

extractBoolean :: Value -> Either [Error Value] Bool
extractBoolean (Boolean b) = Right b
extractBoolean v = Left [Unexpected v]

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
performIO s i = State (io s >> i) (vars s)

createVar :: State -> Var -> State
createVar s v = State (io s) (v : vars s)

findVar :: State -> String -> Maybe Var
findVar (State _ []) _ = Nothing
findVar (State i (x : xs)) str =
  if identifier x == str
    then Just x
    else findVar (State i xs) str

getVar :: State -> String -> Either [Error Value] Value
getVar s name = case findVar s name of
  Nothing -> Left [Undeclared (Text name)]
  Just var -> Right $ val var

removeVar :: [Var] -> String -> [Var]
removeVar [] _ = []
removeVar (x : xs) s =
  if identifier x == s
    then xs
    else x : removeVar xs s

setVar :: State -> Var -> State
setVar s v = case findVar s (identifier v) of
  Nothing -> State (io s) (v : vars s)
  Just _ -> State (io s) (v : removeVar (vars s) (identifier v))

declVar :: State -> String -> Value -> Either [Error Value] State
declVar s name value = case findVar s name of
  Nothing -> Right $ State (io s) (Var name value : vars s)
  Just _ -> Left [VariableAlreadyDeclared (Text name)]
