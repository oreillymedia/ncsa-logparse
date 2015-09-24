import Control.DeepSeq
import Data.Attoparsec.ByteString.Char8
import Data.List (intercalate)
import Options.Applicative (execParser)
import Text.Format (format)


import Analysis
import CommandLineArgs
import File
import Parse.Log
import Types
import Utils



chooseParsingFunction :: CommandLineOpts -> (Parser LogEntry)
chooseParsingFunction args = case parseAsCommon args of
	True  -> parseAsCommonLogLine
	False -> parseAsExtendedLogLine


getLog :: CommandLineOpts -> IO Log
getLog args = do
	logFile <- readLog (logPath args)
	let parserChoice = chooseParsingFunction args
	return $ parseFileLines parserChoice logFile


main :: IO ()
main = do
	args <- execParser opts
	{--
	logFile <- readLog (logPath args)
	let parserChoice = chooseParsingFunction args
	let parsedLog = parseFileLines parserChoice logFile
	--}
	lg <- getLog args
	let totalByteCount = formatInteger $ sumBytes lg
	let total404Count = formatInteger $ countNotFoundResponses lg
	--let intThing = formatInteger $ countNotFoundResponses parsedLog
	--let allOS = intercalate ", " (uniqueBrowsers parsedLog)
	putStrLn $ format "Requests total {0} bytes. 404s: {1}" [totalByteCount, total404Count]
	--putStrLn $ format "Thing: {0}" [intThing]
	return ()
