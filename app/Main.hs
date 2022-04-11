module Main where

import Lexer
import Parser
import ParserBase
import Ast

main :: IO ()
main = print $ case lexString "print \"Hello, world!\";" of
   Left ers -> show ers
   Right toks -> renderAst $ runParser program toks

-- runProgram :: String -> IO ()
-- runProgram s = case runParser program (lexInput s) >>= evaluate . fst of
-- Left ers -> putStrLn $ "Encountered errors: " ++ show ers
-- Right ios -> io ios
