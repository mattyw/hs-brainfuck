module Lib
    ( run
    , alterTape
    , next
    , prev
    , put
    , parens
    , helloWorld
    , otherParen
    ) where

import Data.Char
import Data.List
import Data.Maybe

next c = chr (ord c + 1)
prev c = chr (ord c -1 )

data Program = Program String String Int String Int String String

-- TODO we can remove out
instance Show Program where
    show (Program _ _ _ _ _ _ out) = out

run :: String -> String
run code = run' (Program code code 0 (take 1000 $ repeat '\0') 0 "input" "")

run' :: Program -> String
run' prog = show $ eval prog

-- TODO remove cPos
eval :: Program -> Program
eval (Program [] allCode cPos tape tPos inp out) = Program [] allCode (cPos+1) tape tPos inp out
eval (Program ('>':code) allCode cPos tape tPos inp out) = eval $ Program code allCode (cPos+1) tape (tPos+1) inp out
eval (Program ('<':code) allCode cPos tape tPos inp out) = eval $ Program code allCode (cPos+1) tape (tPos-1) inp out
eval (Program ('+':code) allCode cPos tape tPos inp out) = eval $ Program code allCode (cPos+1) (alterTape tape tPos next) tPos inp out
eval (Program ('-':code) allCode cPos tape tPos inp out) = eval $ Program code allCode (cPos+1) (alterTape tape tPos prev) tPos inp out
eval (Program ('.':code) allCode cPos tape tPos inp out) = eval $ Program code allCode (cPos+1) tape tPos inp (out ++ [tape !! tPos])
eval (Program (',':code) allCode cPos tape tPos (i:inp) out) = eval $ Program code allCode (cPos+1) (alterTape tape tPos (put i)) tPos inp out
-- Jumps
eval (Program ('[':code) allCode cPos tape tPos inp out) = eval $ Program newCode allCode newPos tape tPos inp out
    where
        newPos = jumpForwardPos allCode cPos
        newCode = drop newPos allCode
eval (Program (']':code) allCode cPos tape tPos inp out) = eval $ Program newCode allCode newPos tape tPos inp out
    where
        newPos = jumpBackwardPos allCode cPos
        newCode = drop newPos allCode
-- Ignore all else
eval (Program (_:code) allCode cPos tape tPos inp out) = eval $ Program code allCode (cPos+1) tape tPos inp out

jumpForwardPos :: String -> Int -> Int
jumpForwardPos tape pos =
    if (valueOnTape tape pos) == '\0'
           then  (index+1)
           else pos
    where
        index = otherParen (parens tape) pos

jumpBackwardPos :: String -> Int -> Int
jumpBackwardPos tape pos =
    if (valueOnTape tape pos) /= '\0'
           then  (index+1)
           else pos
    where
        index = otherParen (parens tape) pos

valueOnTape :: String -> Int -> Char
valueOnTape tape pos
    | length(tape) <= pos = '\0'
    | otherwise = tape !! pos

put :: Char -> Char -> Char
put x _ = x

alterTape :: String -> Int -> (Char -> Char) -> String
alterTape "" _ _ = ""
alterTape (x:xs) 0 f = f x : xs
alterTape (x:xs) i f = x : alterTape xs (i-1) f

findParen :: String -> Int -> [Int] -> [(Int,Int)] -> [(Int,Int)]
findParen "" _ [] acc = acc
findParen "" _ (x:xs) acc = error "found open without close"
findParen (']':xs) _ [] _ = error "found close without open"
findParen (']':xs) idx (h:stck) acc = findParen xs (idx+1) stck ((h, idx) : acc)
findParen ('[':xs) idx stck acc = findParen xs (idx+1) (idx : stck) acc
findParen (x:xs) idx stck acc = findParen xs (idx+1) stck acc

parens :: String -> [(Int,Int)]
parens s = findParen s 0 [] []

otherParen :: [(Int, Int)] -> Int -> Int
otherParen [] _ = error "no parens"
otherParen ((a,b):xs) n
    | a == n = b
    | b == n = a
    | otherwise = otherParen xs n

helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
