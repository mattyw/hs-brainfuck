module Lib
    ( run
    , alterTape
    , next
    , prev
    , put
    ) where

import Data.Char

next c = chr (ord c + 1)
prev c = chr (ord c -1 )

data Program = Program String Int String Int String

-- TODO we can remove out
instance Show Program where
    show (Program _ _ tape _ out) = "output: " ++ tape ++ " " ++ out


run :: String -> String
run code = run' (Program code 0 "" 0 "")

run' :: Program -> String
run' prog = show $ eval prog

eval :: Program -> Program
eval (Program ('>':code) cPos tape tPos out) = Program code cPos tape (tPos+1) out
eval (Program ('<':code) cPos tape tPos out) = Program code cPos tape (tPos-1) out
eval (Program ('+':code) cPos tape tPos out) = Program code cPos (alterTape tape tPos next) tPos out
eval (Program ('-':code) cPos tape tPos out) = Program code cPos (alterTape tape tPos prev) tPos out
eval (Program ('.':code) cPos tape tPos out) = Program code cPos tape tPos (out ++ [tape !! tPos])
eval (Program (',':code) cPos tape tPos out) = Program code cPos (alterTape tape tPos (put $ tape !! tPos)) tPos out
-- Ignore all else
eval (Program (_:code) cPos tape tPos out) = Program code cPos tape tPos out


put :: Char -> Char -> Char
put x _ = x

alterTape :: String -> Int -> (Char -> Char) -> String
alterTape "" _ _ = ""
alterTape (x:xs) 0 f = f x : xs
alterTape (x:xs) i f = x : alterTape xs (i-1) f
