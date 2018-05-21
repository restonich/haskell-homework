module Main where

import EvalDelimCC (eval, parsingOps)
import ULC(parse)
import System.IO.Unsafe
import Test.HUnit
import Data.Either
import System.Exit

test1 name str expected =
  TestLabel name $ TestList [c1,c2]
  where
    _  = unsafePerformIO (print rez)
    rez = parse parsingOps str
    c1 = TestCase (assertBool "parsable" (isRight rez))
    (Right tree) = rez
    c2 = TestCase (assertEqual "parsable" expected (eval tree))

tests = TestList
  [ test1 "test1" "(reset (reset 42))"               (Just 42)
  , test1 "test2" "(reset (shift k 42))"             (Just 42)
  , test1 "test3" "(reset (2*(shift k (k 21))))"         (Just 42)
  -- next demo from Wiki
  , test1 "test4" "2*(reset (1+(shift k (k 20))))"   (Just 42)
  , test1 "test5" "(reset (2*(shift k(k 21))))+(reset (shift k (k 42)))" (Just 84)
  , test1 "test7" "42" (Just 42)
  ]

main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
   then
     exitWith ExitSuccess
   else
     exitWith (ExitFailure 1)
