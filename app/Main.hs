--
-- EvalExpr
-- File description:
-- Main
--

module Main (main) where

import System.Environment (getArgs)
import Control.Expression ( Handler (..),
                            catches
                          )
import qualified GHC.Exception as GHCE ( throw )

data ArgException = ArgHelp
                  | ArgError String
deriving (eq)

instance Exception ArgException

instance Show ArgException where
    show ArgHelp = "Usage: evalExpr \"expression\""
    show (ArgError msg) = "Error: " ++ msg

data Token = Help
            | Unknown string
            | Formula String

newtype Expression = Expression String

import Lib

main :: IO ()
main = (getArgs >>= evalExpr) `catches` [ Handler  ]

handlerHelp :: ArgException -> IO ()


evalExpr :: [String] -> IO ()
evalExpr [] = putStrLn "Error: No argument"

tokenizeArgToToken :: [String] -> [Token]
tokenize [] = []
tokenize ("-h":       xs) = Help        : tokenize xs
tokenize (opt@('-':_):xs) = Unknown opt : tokenize xs
tokenize (x:xs)           = Formula x   : tokenize xs

parseTokenToExpression :: [Token] -> Expression
parseTokenToExpression = 

parseTokenNoArg :: [Token] -> Bool
parseTokenNoArg [] = GHCE.throw "Requires at least one expression, retry with '-h'"
parseTokenNoArg _  = True

ParseTokenHelp :: [Token] -> Bool
ParseTokenHelp Help = GHCE.throw "Usage: ./eval_expr expr [expr ...]"
ParseTokenHelp _    = True
