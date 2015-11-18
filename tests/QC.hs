module Main (main) where

import qualified QC.DateTime as DateTime
import qualified QC.HTTP as HTTP
import qualified QC.Integration as Integration
import Test.Framework (defaultMain, testGroup, Test)


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [
      testGroup "HTTP" HTTP.tests
    , testGroup "Date/Time" DateTime.tests
    , testGroup "Integration" Integration.tests
  ]