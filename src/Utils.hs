module Utils where

import Control.Monad (unless)
import Data.List.Split
import Data.List
import System.Directory (doesFileExist)


-- |Check if the given path points to a real file. Stop execution and displays an error message if not.
verifyFilePath :: FilePath -> IO ()
verifyFilePath pth = do
	fileExists <- doesFileExist pth
	unless fileExists $ error ("ERROR: The file " ++ pth ++ " does not exist.") 


-- | Format a number in the US English locale (commas separating thousands)
formatInteger :: Int -> String
formatInteger = reverse . (intercalate ",") . (chunksOf 3) . reverse . show


