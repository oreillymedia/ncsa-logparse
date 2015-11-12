module File (readLog ) where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import System.Directory (doesFileExist)



-- | Read the file at the given path and return as a list of lazy bytestrings.
readLog :: FilePath -> IO [BL.ByteString]
readLog path = verifyFilePath path >> BL.readFile path  >>= return . BL8.lines


-- |Check if the given path points to a real file. Stop execution and displays an error message if not.
verifyFilePath :: FilePath -> IO ()
verifyFilePath pth = do
	fileExists <- doesFileExist pth
	unless fileExists $ error ("ERROR: The file " ++ pth ++ " does not exist.")
