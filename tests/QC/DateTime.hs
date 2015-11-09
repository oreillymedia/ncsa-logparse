module QC.DateTime (tests) where

import Data.Either
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Data.Time.LocalTime (minutesToTimeZone)
import System.Locale (defaultTimeLocale, months)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Parse.DateTime
import Types


tests = [
      testProperty "propTZParse" propTZParse
    , testProperty "propMonthMap" propMonthMap
  ]


testParser :: Eq b => [B.ByteString] -> [b] -> Parser b -> Bool
testParser inputs outputs parser = (rights $ map (parseOnly parser) inputs) == outputs


propTZParse :: Bool
propTZParse =
                testParser
                ["07", "+07", "-07", "-0730", "AB"]
                [ minutesToTimeZone $ 60 * 7
                , minutesToTimeZone $ 60 * 7
                , minutesToTimeZone $ 60 * 7 * (-1)
                , minutesToTimeZone $ 60 * 7 * (-1) - 30]
                parseTZ


propMonthMap :: Bool
propMonthMap = expected == actual
    where
        monthNames = map snd (months defaultTimeLocale) ++ ["smarch"]
        actual = map (monthNameToNumber) monthNames
        expected =  map Just [1..12] ++ [Nothing]
