module Token where

data ReservedTokenType
  = BANG_EQUAL
  | EQUAL_EQUAL
  | GREATER_EQUAL
  | LESS_EQUAL
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | BANG
  | EQUAL
  | GREATER
  | LESS
  | AND
  | CLASS
  | ELSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | VAR
  | WHILE
  | TRUE
  | FALSE
  | WHITESPACE
  | EOF
  deriving (Show, Enum, Eq, Ord, Bounded)

type TokenContext = Int

data Token
  = IdentifierToken TokenContext String
  | ReservedToken TokenContext ReservedTokenType
  | StringToken TokenContext String
  | NumberToken TokenContext Float 
  deriving (Show, Eq)

isReservedTokenOfType :: ReservedTokenType -> Token -> Bool
isReservedTokenOfType tt (ReservedToken _ t) = t == tt
isReservedTokenOfType _ _ = False

isStringToken :: Token -> Bool
isStringToken (StringToken _ _) = True
isStringToken _ = False

isIdToken :: Token -> Bool
isIdToken (IdentifierToken _ _) = True
isIdToken _ = False

isNumberToken :: Token -> Bool
isNumberToken (NumberToken _ _) = True
isNumberToken _ = False
