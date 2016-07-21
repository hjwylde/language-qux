{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Main where

import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" []
