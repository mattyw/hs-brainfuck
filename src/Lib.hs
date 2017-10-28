module Lib
    ( run
    , alterTape
    , next
    , prev
    , put
    ) where

import Data.Char
import Data.List
import Data.Maybe

next c = chr (ord c + 1)
prev c = chr (ord c -1 )

data Program = Program String Int String Int String String

-- TODO we can remove out
instance Show Program where
    show (Program code _ tape _ _ out) = "output: " ++ code ++ tape ++ " " ++ out


run :: String -> String
run code = run' (Program code 0 "" 0 "input" "")

run' :: Program -> String
run' prog = show $ eval prog

eval :: Program -> Program
eval (Program [] cPos tape tPos inp out) = Program [] cPos tape (tPos+1) inp out
eval (Program ('>':code) cPos tape tPos inp out) = eval $ Program code cPos tape (tPos+1) inp out
eval (Program ('<':code) cPos tape tPos inp out) = eval $ Program code cPos tape (tPos-1) inp out
eval (Program ('+':code) cPos tape tPos inp out) = eval $ Program code cPos (alterTape tape tPos next) tPos inp out
eval (Program ('-':code) cPos tape tPos inp out) = eval $ Program code cPos (alterTape tape tPos prev) tPos inp out
eval (Program ('.':code) cPos tape tPos inp out) = eval $ Program code cPos tape tPos inp (out ++ [tape !! tPos])
eval (Program (',':code) cPos tape tPos (i:inp) out) = eval $ Program code cPos (alterTape tape tPos (put i)) tPos inp out
-- Jumps
eval (Program ('[':code) cPos tape tPos inp out) = eval $ jumpForward $ Program code cPos tape tPos inp out
eval (Program (']':code) cPos tape tPos inp out) = eval $ jumpBackward $ Program code cPos tape tPos inp out
-- Ignore all else
eval (Program (_:code) cPos tape tPos inp out) = eval $ Program code cPos tape tPos inp out


valueOnTape :: String -> Int -> Char
valueOnTape tape pos
    | length(tape) < pos = '\0'
    | otherwise = tape !! pos

jumpForwardPos :: String -> Int -> Maybe Int
jumpForwardPos tape pos = 
    if (valueOnTape tape pos) == '\0' 
        then elemIndex ']' tape
        else Just pos

jumpBackwardPos :: String -> Int -> Maybe Int
jumpBackwardPos tape pos = 
    if (valueOnTape tape pos) != '\0' 
        then elemIndex ']' tape -- need to implement jump back
        else Just pos

jumpForward :: Program -> Program
jumpForward (Program code cPos tape tPos inp out) = Program code cPos tape (fromJust (jumpForwardPos tape tPos)) inp out

jumpBackward :: Program -> Program
jumpForward (Program code cPos tape tPos inp out) = Program code cPos tape (fromJust (jumpBackwardPos tape tPos)) inp out

put :: Char -> Char -> Char
put x _ = x

alterTape :: String -> Int -> (Char -> Char) -> String
alterTape "" _ _ = ""
alterTape (x:xs) 0 f = f x : xs
alterTape (x:xs) i f = x : alterTape xs (i-1) f
