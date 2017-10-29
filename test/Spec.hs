import Test.HUnit

import Lib
import System.Exit
import Control.Monad

testMatch = TestCase (do
    let out = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    assertEqual "Test match" (Lib.parens out) [(8,48),(43, 45),(14, 33)]
    )
tests = TestList [
    testMatch
    ]
main :: IO ()
main = do
    counts <- runTestTT tests
    when (failures counts > 0 || errors counts > 0)
        exitFailure
