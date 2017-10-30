import Test.HUnit

import Lib
import System.Exit
import Control.Monad

testMatch = TestCase (do
    let out = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    let parens = [(8,48),(43, 45),(14, 33)]
    assertEqual "Test match" parens (Lib.parens out)
    assertEqual "Find other" 48 (otherParen parens 8)
    assertEqual "Find other" 43 (otherParen parens 45)
    )
testRun = TestCase (do
    let bang = "+++++++++++++++++++++++++++++++++.+."
    let program = "++++++++++++++++++++++++++++++++++++++++++++++++++++.>++++++++++++++++++++++++++++++++++++++++++++++++++++-<+++++++++++++++++++++++++++++++++++++++++++++++++++-.>>,."
    let helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    let onejump = "[+++++++++++++++++++++++++++++++++.]+++++++++++++++++++++++++++++++++."
    assertEqual "Test bang" "!\"" (run bang)
    assertEqual "Test 4fi" "4fi" (run program)
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
    let jump = "[+]++"
    let prog = (Program jump jump 0 (take 10 $ repeat '\0') 0 "" "")
    let expected = (Program jump jump 0 "\STX\0\0\0\0\0\0\0\0\0" 0 "" "")
    assertEqual "jump result" (tape expected) (tape (eval prog))
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
