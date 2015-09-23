import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (intercalate, nub)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Options.Applicative
import Text.Format (format)
import Web.UAParser (osrFamily, uarFamily)

import CommandLineArgs
import Parse.Log
import Types
import Utils


lazyToStrict ::  BL8.ByteString -> B.ByteString
lazyToStrict = B.concat . BL.toChunks


readLogFile :: FilePath -> IO [BL.ByteString]
readLogFile path = verifyFilePath path >> BL.readFile path  >>= return . BL8.lines


sumBytes :: Log -> Int
sumBytes commonLog = sum $ map byteSize commonLog


uniqueOSFamilies :: Log -> [String]
uniqueOSFamilies logEntries = map T.unpack allPlatforms
	where
		allPlatforms = nub $ map osrFamily $ catMaybes $ map platform logEntries


uniqueBrowsers :: Log -> [String]
uniqueBrowsers logEntries = map T.unpack allPlatforms
	where
		allPlatforms = nub $ map uarFamily $ catMaybes $ map browser logEntries




main :: IO ()
main = do
	args <- execParser opts
	logFile <- readLogFile (logPath args)
	let parserChoice = chooseParsingFunction args
	let parsedLog = parseFileLines parserChoice logFile
	let totalByteCount = formatInteger $ sumBytes parsedLog
	let allOS = intercalate ", " (uniqueBrowsers parsedLog)
	let lineCount = formatInteger (length parsedLog)
	putStrLn $ format "{0} valid lines in file. Requests total {1} bytes. All OS families: {2}" [lineCount, totalByteCount, allOS]
	return ()
