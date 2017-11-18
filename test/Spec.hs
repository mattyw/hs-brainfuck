import Test.HUnit

import Lib
import System.Exit
import Control.Monad
import qualified Data.List.Zipper as Z
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

testMatch = TestCase (do
    let out = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    let parens = Lib.parens out
    assertEqual "Find other" 48 (otherParen parens 8)
    assertEqual "Find other" 43 (otherParen parens 45)
    )
testRun = TestCase (do
    let bang = "+++++++++++++++++++++++++++++++++.+."
    let program = "++++++++++++++++++++++++++++++++++++++++++++++++++++.>++++++++++++++++++++++++++++++++++++++++++++++++++++-<+++++++++++++++++++++++++++++++++++++++++++++++++++-.>>,."
    let helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    let onejump = "[+++++++++++++++++++++++++++++++++.]+++++++++++++++++++++++++++++++++."
    assertEqual "Test bang" "!\"" (run bang)
    assertEqual "Test 4fi" "4fa" (run program)
    assertEqual "Test " "!" (run onejump)
    assertEqual "Test " "Hello World!\n" (run helloWorld)
    )
testJumpPos = TestCase (do
    let jump = "[+]++"
    assertEqual "jump pos" 3 (jumpForwardPos (Lib.parens jump) '\0' 0)
    assertEqual "jump pos" 1 (jumpForwardPos (Lib.parens jump) '1' 0)
    assertEqual "jump pos" 3 (jumpBackwardPos (Lib.parens jump) '\0' 2)
    assertEqual "jump pos" 1 (jumpBackwardPos (Lib.parens jump) '1' 2)
    )
testEval = TestCase (do
    let one = "+."
    let prog = (Program 0 (Z.fromList (replicate 10 '\0')) "" S.empty)
    let expected = (Program 0 (Z.fromList "\SOH\0\0\0\0\0\0\0\0\0") "" S.empty)
    assertEqual "one result" (tape expected) (tape (eval prog (Lib.code one) (length one) (parens one)))
    let jump = "[+]++"
    let prog = (Program 0 (Z.fromList (replicate 10 '\0')) "" S.empty)
    let expected = (Program 0 (Z.fromList "\STX\0\0\0\0\0\0\0\0\0") "" S.empty)
    assertEqual "jump result" (tape expected) (tape (eval prog (Lib.code jump) (length jump) (parens jump)))
    )
tests = TestList [
    testMatch
    , testJumpPos
    , testEval
    , testRun
    ]
main :: IO ()
main = do
    counts <- runTestTT tests
    when (failures counts > 0 || errors counts > 0)
        exitFailure
