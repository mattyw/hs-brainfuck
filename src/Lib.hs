module Lib
    ( run
    , alterTape
    , next
    , prev
    , insertKeys
    , jumpForwardPos
    , jumpBackwardPos
    , tape
    , parens
    , otherParen
    , Program (Program)
    , eval
    ) where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.List.Zipper as Z
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Foldable as F (toList)

next :: Char -> Char
next '\255' = chr 0
next c = chr (ord c + 1)

prev :: Char -> Char
prev '\0' = chr 255
prev c = chr (ord c -1 )

type Tape = Z.Zipper Char
type Output = S.Seq Char

-- TODO Remove TapePosition
data Program = Program Int Tape String Output

-- TODO we can remove out
instance Show Program where
    show (Program _ _ _ out) = F.toList out

instance Eq Program where
    (==) (Program a1 a2 a3 a4)
         (Program b1 b2 b3 b4) = a1 == b1 &&
                                          a2 == b2 &&
                                          a3 == b3 &&
                                          a4 == b4
tape :: Program -> String
tape (Program _ t _ _) = show t

run :: String -> String
run commentedCode = run' (Program 0 (Z.fromList (replicate 30000 '\0')) "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz" S.empty) code (parens code)
    where
        code = filter (`elem` "><+-,.[]") commentedCode

run' :: Program -> String -> M.Map Int Int -> String
run' prog code parens = show $ eval prog code (length code) (parens)

eval :: Program -> String -> Int -> M.Map Int Int -> Program
eval (Program cPos tape inp out) code progLen parens = case (cPos >= progLen) of
    True -> Program cPos tape inp out
    otherwise -> eval (op (code !! cPos) (Program cPos tape inp out) parens) code progLen parens

-- TODO remove cPos
-- TODO Code as zipper list
op :: Char -> Program -> M.Map Int Int -> Program
op '>' (Program cPos tape inp out) parens = (Program (cPos+1) (Z.right tape) inp out)
op '<' (Program cPos tape inp out) parens = (Program (cPos+1) (Z.left tape) inp out)
op '+' (Program cPos tape inp out) parens = (Program (cPos+1) (alterTape tape next) inp out)
op '-' (Program cPos tape inp out) parens = (Program (cPos+1) (alterTape tape prev) inp out)
op '.' (Program cPos tape inp out) parens = (Program (cPos+1) tape inp (out S.|> (valueOnTape tape))) 
op ',' (Program cPos tape (i:inp) out) parens = (Program (cPos+1) (alterTape tape (\_ -> i)) inp out)
-- Jumps
op '[' (Program cPos tape inp out) parens = (Program (jumpForwardPos parens (valueOnTape tape) cPos) tape inp out)
op ']' (Program cPos tape inp out) parens = (Program (jumpBackwardPos parens (valueOnTape tape) cPos) tape inp out) 
-- Ignore all else
op _ (Program cPos tape inp out) parens = (Program (cPos+1) tape inp out) 

jumpForwardPos :: M.Map Int Int -> Char -> Int -> Int
jumpForwardPos parens tape pos =
    if tape == '\0'
           then (otherParen parens pos)+1
           else (pos+1)

jumpBackwardPos :: M.Map Int Int -> Char -> Int -> Int
jumpBackwardPos parens tape pos =
    if tape /= '\0'
           then (otherParen parens pos)+1
           else (pos+1)

valueOnTape tape = Z.cursor tape

alterTape tape f = Z.replace (f $ Z.cursor tape) tape

findParen :: String -> Int -> [Int] -> M.Map Int Int -> M.Map Int Int
findParen "" _ [] acc = acc
findParen "" _ (x:xs) acc = error "found open without close"
findParen (']':xs) _ [] _ = error "found close without open"
findParen (']':xs) idx (h:stck) acc = findParen xs (idx+1) stck (insertKeys h idx acc)
findParen ('[':xs) idx stck acc = findParen xs (idx+1) (idx : stck) acc
findParen (x:xs) idx stck acc = findParen xs (idx+1) stck acc

insertKeys :: Int -> Int -> M.Map Int Int -> M.Map Int Int
insertKeys key value m = M.insert value key $ M.insert key value m

parens :: String -> M.Map Int Int
parens s = findParen s 0 [] M.empty

otherParen :: M.Map Int Int -> Int -> Int
otherParen = (M.!)
