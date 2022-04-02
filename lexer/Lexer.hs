module Lexer where

import ParserBase
import Control.Applicative
import Token

--  IdentifierToken String TokenContext
--  ReservedToken ReservedTokenType TokenContext
--  StringToken String TokenContext
--  NumberToken Float TokenContext
--  WhitespaceToken WhitespaceTokenType TokenContext

stringToken :: TokenContext -> Parser String Token
stringToken c = do
  _ <- match "\""
  text <- concat <$> many (satisfy (/= "\""))
  _ <- match "\""
  return $ StringToken c text