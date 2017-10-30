module Lib
    ( run
    , alterTape
    , next
    , prev
    , jumpForwardPos
    , jumpBackwardPos
    , tape
    , put
    , parens
    , otherParen
    , Program (Program)
    , eval
    ) where

import Data.Char
import Data.List
import Data.Maybe

next :: Char -> Char
next '\255' = chr 0
next c = chr (ord c + 1)

prev :: Char -> Char
prev '\0' = chr 255
prev c = chr (ord c -1 )

data Program = Program String String Int String Int String String

-- TODO we can remove out
instance Show Program where
    show (Program _ _ _ _ _ _ out) = out

instance Eq Program where
    (==) (Program a1 a2 a3 a4 a5 a6 a7)
         (Program b1 b2 b3 b4 b5 b6 b7) = a1 == b1 &&
                                          a2 == b2 &&
                                          a3 == b3 &&
                                          a4 == b4 &&
                                          a5 == b5 &&
                                          a6 == b6 &&
                                          a7 == b7
tape :: Program -> String
tape (Program _ _ _ t _ _ _) = t

run :: String -> String
run code = run' (Program code code 0 (take 30000 $ repeat '\0') 0 "abcdefghijklmnopqrstuvwxyz" "")

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
        newPos = jumpForwardPos (parens allCode) (valueOnTape tape tPos) cPos
        newCode = drop newPos allCode
eval (Program (']':code) allCode cPos tape tPos inp out) = eval $ Program newCode allCode newPos tape tPos inp out
    where
        newPos = jumpBackwardPos (parens allCode) (valueOnTape tape tPos) cPos
        newCode = drop newPos allCode
-- Ignore all else
eval (Program (_:code) allCode cPos tape tPos inp out) = eval $ Program code allCode (cPos+1) tape tPos inp out

jumpForwardPos :: [(Int,Int)] -> Char -> Int -> Int
jumpForwardPos parens tape pos =
    if tape == '\0'
           then (index+1)
           else (pos+1)
    where
        index = otherParen parens pos

jumpBackwardPos :: [(Int,Int)] -> Char -> Int -> Int
jumpBackwardPos parens tape pos =
    if tape /= '\0'
           then (index+1)
           else (pos+1)
    where
        index = otherParen parens pos

valueOnTape :: String -> Int -> Char
valueOnTape tape pos = tape !! pos

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

-- TODO the tape should be Ints or Bytes
parens :: String -> [(Int,Int)]
parens s = findParen s 0 [] []

otherParen :: [(Int, Int)] -> Int -> Int
otherParen [] _ = error "no parens"
otherParen ((a,b):xs) n
    | a == n = b
    | b == n = a
    | otherwise = otherParen xs n
