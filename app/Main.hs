module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    prog <- readFile $ head args
    putStrLn $ Lib.run prog
