module Main where

import Lexer
import Parser
import ParserBase

main :: IO ()
main = print $ case lexString "print 5 * 6;" of
   Left ers -> show ers
   Right toks -> case runParser program toks of
     Left ers -> show ers
     Right ast -> show ast

-- runProgram :: String -> IO ()
-- runProgram s = case runParser program (lexInput s) >>= evaluate . fst of
-- Left ers -> putStrLn $ "Encountered errors: " ++ show ers
-- Right ios -> io ios
