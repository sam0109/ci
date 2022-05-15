module Main where

import Lexer
import Parser
import ParserBase

import Interpreter
import State

main :: IO ()
main = case lexString "var a = 5; a = 10; print a;" of
   Left ers -> print ers
   Right toks -> do
      putStr $ "\n\n\nLexer output: \n  " ++ show toks ++ "\n"
      case runParser program toks of
        Left ers -> print ers
        Right (programAst, []) -> do
            putStr $ "Parser output: \n  " ++ show programAst ++ "\n"
            case evalProgram programAst of
              Left ers -> print ers
              Right (State ioResult _) -> putStr "Interpreter output: \n  " >> ioResult
        Right (_, xs) -> print ("Unconsumed tokens: " ++ show xs)

-- runProgram :: String -> IO ()
-- runProgram s = case runParser program (lexInput s) >>= evaluate . fst of
-- Left ers -> putStrLn $ "Encountered errors: " ++ show ers
-- Right ios -> io ios
