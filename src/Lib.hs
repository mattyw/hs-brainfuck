module Lib
    ( run
    , alterTape
    , next
    , runM
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
import Control.Monad.State
import Debug.Trace (traceShow)

next :: Char -> Char
next '\255' = chr 0
next c = chr (ord c + 1)

prev :: Char -> Char
prev '\0' = chr 255
prev c = chr (ord c -1 )

type Tape = Z.Zipper Char
type Output = S.Seq Char

-- TODO Remove TapePosition
data Program = Program String Int Tape String Output

-- TODO we can remove out
instance Show Program where
    show (Program _ _ _ _ out) = F.toList out

instance Eq Program where
    (==) (Program a1 a2 a3 a4 a5)
         (Program b1 b2 b3 b4 b5) = a1 == b1 &&
                                          a2 == b2 &&
                                          a3 == b3 &&
                                          a4 == b4 &&
                                          a5 == b5
tape :: Program -> String
tape (Program _ _ t _ _) = show t

code :: Program -> String
code (Program c _ _ _ _) = c

cpos :: Program -> Int
cpos (Program _ p _ _ _) = p

run :: String -> String
run commentedCode = run' (Program code 0 (Z.fromList (replicate 30000 '\0')) "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz" S.empty) (parens code)
    where
        code = filter (`elem` "><+-,.[]") commentedCode

run' :: Program -> M.Map Int Int -> String
run' prog parens = show $ eval prog (length $ code prog) (parens)

eval :: Program -> Int -> M.Map Int Int -> Program
eval (Program code cPos tape inp out) progLen parens = case (cPos >= progLen) of
    True -> Program code cPos tape inp out
    otherwise -> eval (op (code !! cPos) (Program code cPos tape inp out) parens) progLen parens

evalM :: Int -> Int -> M.Map Int Int -> State Program String
evalM cPos progLen parens =
    case (cPos >= (progLen-1)) of
        True -> do
            prog <- get
            return $ show prog
        otherwise -> do
            (Program code cPos tape inp out) <- get
            let newProg = op (code !! cPos) (Program code cPos tape inp out) parens
            --put $ traceShow [(progLen-1), cpos newProg]  newProg
            put newProg
            evalM (cpos newProg) progLen parens

runM :: String -> IO ()
runM commentedCode = print $ evalState (evalM 0 (length code) (parens code)) (Program code 0 (Z.fromList (replicate 30000 '\0')) "" S.empty)
    where
        code = filter (`elem` "><+-,.[]") commentedCode

-- TODO remove cPos
-- TODO Code as zipper list
op :: Char -> Program -> M.Map Int Int -> Program
op '>' (Program code cPos tape inp out) parens = (Program code (cPos+1) (Z.right tape) inp out)
op '<' (Program code cPos tape inp out) parens = (Program code (cPos+1) (Z.left tape) inp out)
op '+' (Program code cPos tape inp out) parens = (Program code (cPos+1) (alterTape tape next) inp out)
op '-' (Program code cPos tape inp out) parens = (Program code (cPos+1) (alterTape tape prev) inp out)
op '.' (Program code cPos tape inp out) parens = (Program code (cPos+1) tape inp (out S.|> (valueOnTape tape))) 
op ',' (Program code cPos tape (i:inp) out) parens = (Program code (cPos+1) (alterTape tape (\_ -> i)) inp out)
-- Jumps
op '[' (Program code cPos tape inp out) parens = (Program code (jumpForwardPos parens (valueOnTape tape) cPos) tape inp out)
op ']' (Program code cPos tape inp out) parens = (Program code (jumpBackwardPos parens (valueOnTape tape) cPos) tape inp out) 
-- Ignore all else
op _ (Program code cPos tape inp out) parens = (Program code (cPos+1) tape inp out) 

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

--put :: Char -> Char -> Char
--put x _ = x

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
