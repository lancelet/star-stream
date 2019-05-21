{-|
Module      : Test
Description : Test suite for the gaia-fetch package.
-}
module Main where

import qualified Test.DocTest as DocTest
import qualified Test.Tasty   as Tasty

main :: IO ()
main = doctests
    >> Tasty.defaultMain tests


tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests"
  [
  ]


doctests :: IO ()
doctests = do
  putStrLn "\nRunning doctests ..."
  DocTest.doctest
    [ "-isrc"
    , "src/GaiaFetch.hs"
    ]
  putStrLn "... done running doctests.\n"
