module Main where

import Lexer (lexInput)
import Parser
import Interpreter

main :: IO ()
main = runProgram "print \"Hello, world!\"; print \"Hello, Chris!\"; print \"Hello, Sam!\";"

runProgram :: String -> IO ()
runProgram s = case runParser program (lexInput s) >>= evaluate . fst of
  Left ers -> putStrLn $ "Encountered errors: " ++ show ers
  Right ios -> io ios
