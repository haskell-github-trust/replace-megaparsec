{-# LANGUAGE BlockArguments #-}

module Main ( main ) where

import Test.Hspec
import TestString
import TestByteString
import TestText

main :: IO ()
main = hspec do
  TestString.tests
  TestByteString.tests
  TestText.tests
