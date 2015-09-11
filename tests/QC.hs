module Main (main) where

import qualified QC.HTTP as HTTP
import Test.Framework (defaultMain, testGroup)

main = defaultMain tests


tests = [
    testGroup "HTTP" HTTP.tests
  ]