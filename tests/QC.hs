module Main (main) where

import qualified QC.DateTime as DateTime
import qualified QC.HTTP as HTTP
import qualified QC.Integration as Integration
import Test.Framework (defaultMain, testGroup)

main = defaultMain tests


tests = [
      testGroup "HTTP" HTTP.tests
    , testGroup "Date/Time" DateTime.tests
    , testGroup "Integration" Integration.tests
  ]