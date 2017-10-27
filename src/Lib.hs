module Lib
    ( run
    , alterTape
    , next
    , prev
    ) where

import Data.Char

next c = chr (ord c + 1)
prev c = chr (ord c -1 )

data Program = Program String Int String Int String deriving (Show)


run :: String -> String
run code = run' (Program code 0 "" 0 "")

run' :: Program -> String
run' prog = ""

eval :: Program -> Program
eval (Program ('+':code) cPos tape tPos out) = Program code cPos tape (tPos+1) out
eval (Program ('-':code) cPos tape tPos out) = Program code cPos tape (tPos-1) out
eval (Program ('>':code) cPos tape tPos out) = Program code cPos (alterTape tape tPos next) tPos out
eval (Program ('<':code) cPos tape tPos out) = Program code cPos (alterTape tape tPos prev) tPos out
eval (Program ('.':code) cPos tape tPos out) = Program code cPos tape tPos (out ++ [tape !! tPos])
-- Don't understand what to put onto the tape, need to read about brain fuck
eval (Program (',':code) cPos tape tPos out) = Program code cPos (alterTape tape tPos id c) tPos out

alterTape :: String -> Int -> (Char -> Char) -> String
alterTape "" _ _ = ""
alterTape (x:xs) 0 f = f x : xs
alterTape (x:xs) i f = x : alterTape xs (i-1) f
