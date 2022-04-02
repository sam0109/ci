module Error where

data Error i
  = EndOfInput -- Expected more input, but there is nothing
  | Unexpected i -- We encountered an element we weren't expecting
  | InvalidOperation i -- An operation is performed on invalid types
  | UnsupportedValue i -- A terminal contains an unsupported token
  | VariableAlreadyDeclared i
  deriving (Eq)

instance (Show i) => Show (Error i) where
  show EndOfInput = "Unexpectedly reached the end of input."
  -- show (Expected got expected) = "Expected: \"" ++ show expected ++ "\", but instead found: \"" ++ show got ++ "\"."
  show (Unexpected got) = "Unexpectedly found: \"" ++ show got ++ "\"."
  show (InvalidOperation t) = "An operation was performed on invalid types. Token: " ++ show t
  show (UnsupportedValue t) = "A terminal contains an unsupported token. Token: " ++ show t
  show (VariableAlreadyDeclared s) = "Found a variable declaration for a variable that was already declared: " ++ show s
