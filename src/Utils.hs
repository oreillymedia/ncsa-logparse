module Utils where

import Prelude hiding (filter, map)
import Data.List (intercalate)
import Data.List.Split
import Data.Time (ZonedTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)


-- | Format a number in the US English locale (commas separating thousands)
formatInteger :: Int -> String
formatInteger = reverse . (intercalate ",") . (chunksOf 3) . reverse . show

