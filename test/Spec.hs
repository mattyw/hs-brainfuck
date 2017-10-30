import Test.HUnit

import Lib
import System.Exit
import Control.Monad

testMatch = TestCase (do
    let out = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    let parens = [(8,48),(43, 45),(14, 33)]
    assertEqual "Test match" parens (Lib.parens out)
    assertEqual "Find other" 48 (Lib.otherParen parens 8)
    assertEqual "Find other" 43 (Lib.otherParen parens 45)
    )
testRun = TestCase (do
    let bang = "+++++++++++++++++++++++++++++++++.+."
    let program = "++++++++++++++++++++++++++++++++++++++++++++++++++++.>++++++++++++++++++++++++++++++++++++++++++++++++++++-<+++++++++++++++++++++++++++++++++++++++++++++++++++-.>>,."
    assertEqual "Test bang" "!\"" (run bang)
    assertEqual "Test 4fi" "4fi" (run program)
    )
tests = TestList [
    testMatch
    , testRun
    ]
main :: IO ()
main = do
    counts <- runTestTT tests
    when (failures counts > 0 || errors counts > 0)
        exitFailure
