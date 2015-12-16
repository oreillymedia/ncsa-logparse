module QC.DateFormatter (tests) where

import Data.Either
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Data.Time

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import DateFormatter


tests :: [Test]
tests = [
      testProperty "Test that date formatting works" testDateFormat
 ]


testDateFormat :: Bool
testDateFormat = formatZonedTime zt == "2015-01-15 13:33:20 +0000"
    where
        lt = LocalTime {localDay = fromGregorian 2015 1 15, localTimeOfDay = TimeOfDay 13 33 20}
        zt = ZonedTime {zonedTimeToLocalTime = lt, zonedTimeZone = utc}

