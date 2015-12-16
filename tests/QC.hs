module Main (main) where

import Test.Framework (defaultMain, testGroup, Test)

import qualified QC.DateFormatter as DateFormatter
import qualified QC.DateTime as DateTime
import qualified QC.HTTP as HTTP
import qualified QC.Integration as Integration


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [
      testGroup "HTTP" HTTP.tests
    , testGroup "Date/Time" DateTime.tests
    , testGroup "Datetime Formatting" DateFormatter.tests
    , testGroup "Integration" Integration.tests
  ]