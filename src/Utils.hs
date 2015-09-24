module Utils where

import Prelude hiding (filter, map)
import Control.Monad (unless)
import Data.Either (isRight)
import Data.List (intercalate)
import Data.List.Split
import Data.List.Stream (filter, map)
import Data.Maybe (fromJust, isJust)
import System.Directory (doesFileExist)


-- |Check if the given path points to a real file. Stop execution and displays an error message if not.
verifyFilePath :: FilePath -> IO ()
verifyFilePath pth = do
	fileExists <- doesFileExist pth
	unless fileExists $ error ("ERROR: The file " ++ pth ++ " does not exist.") 


-- | Format a number in the US English locale (commas separating thousands)
formatInteger :: Int -> String
formatInteger = reverse . (intercalate ",") . (chunksOf 3) . reverse . show


{--

			Stream fusion-friendly versions of some common functions.

--}



-- | A stream fustion-friendly version of Data.Maybe.catMaybes
catMaybes :: [Maybe a] -> [a]
catMaybes ls = map fromJust (filter isJust ls)


fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = error "No right value."


-- | A stream fustion-friendly version of Data.Either.rights
rights :: [Either a b] -> [b]
rights es = map fromRight (filter isRight es)

