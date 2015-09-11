import Data.Either
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Options.Applicative
import Text.Format (format)

import CommandLineArgs
import Parse.Log
import Types
import Utils


lazyToStrict ::  BL8.ByteString -> B.ByteString
lazyToStrict = B.concat . BL.toChunks


parseFileLines :: [BL.ByteString] -> Log
parseFileLines = rights . (map  parseFileLine)


parseFileLine :: BL.ByteString -> Either String LogEntry
parseFileLine logFileLine = ln >>= parseOnly parseLogLine
    where
        ln = (Right . B.concat . BL.toChunks) logFileLine


totalBytes :: Log -> Int
totalBytes logline = sum $ map byteSize logline


readLogFile :: FilePath -> IO [BL.ByteString]
readLogFile path = verifyFilePath path >> BL.readFile path  >>= return . BL8.lines


sumBytes :: (Num a) => (LogEntry -> a) -> [BL.ByteString] -> a
sumBytes fn logLines = sum $ map extractBytes logLines
	where extractBytes = (either (\_ -> 0) fn) . (parseOnly parseLogLine) . lazyToStrict





main :: IO ()
main = do
	args <- execParser opts
	logFile <- readLogFile (logPath args)
	let totalByteCount = formatInteger $ sumBytes byteSize logFile
	let parsedLog = parseFileLines logFile
	let lineCount = formatInteger (length parsedLog)
	let totalByteCount = formatInteger (totalBytes parsedLog)
	putStrLn $ format "{0} valid lines in file. Requests total {1} bytes" ["0", totalByteCount]
	return ()
