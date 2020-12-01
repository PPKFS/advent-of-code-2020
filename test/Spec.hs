module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Relude
import AdventOfCode

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = makeTests [day1]

day1 :: IO ()
day1 = do
    putStrLn "Day1 Part 1:"
    doDay1 2
    putStrLn "Day1 Part 2:"
    doDay1 3

makeTests :: [Assertion] -> TestTree
makeTests lst = testGroup "Tests" $ zipWith
  (curry
     (\ (i, x)
        -> after AllFinish (mkName (i - 1)) $ testCase (mkName i) x))
  [1 .. ]
  lst
    where mkName = show