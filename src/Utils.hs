module Utils where

import Prelude hiding (filter, map)
import Control.Monad (unless)
import Data.List (intercalate)
import Data.List.Split
import Data.Time (ZonedTime)
import Data.Time.Format (formatTime)
import System.Directory (doesFileExist)
import System.Locale (defaultTimeLocale)


-- |Check if the given path points to a real file. Stop execution and displays an error message if not.
verifyFilePath :: FilePath -> IO ()
verifyFilePath pth = do
	fileExists <- doesFileExist pth
	unless fileExists $ error ("ERROR: The file " ++ pth ++ " does not exist.") 


-- | Format a number in the US English locale (commas separating thousands)
formatInteger :: Int -> String
formatInteger = reverse . (intercalate ",") . (chunksOf 3) . reverse . show


-- | Format zoned time as a string in ISO-8601 format.
formatZonedTime :: ZonedTime -> String
formatZonedTime = formatTime defaultTimeLocale "%FT%T%QZ"

