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
tests = TestList [
    testMatch
    ]
main :: IO ()
main = do
    counts <- runTestTT tests
    when (failures counts > 0 || errors counts > 0)
        exitFailure
