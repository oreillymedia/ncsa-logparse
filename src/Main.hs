import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Lazy as BL
import Options.Applicative (execParser)
import System.IO (hPutStrLn, stdout, stderr)

import Analysis
import Csv
import CommandLineArgs
import File
import Parse.DateTime
import Parse.Log
import Types


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
	logData <- getLog args
	let csv = toCSV logData
	BL.hPut stdout csv
	hPutStrLn stderr "Conversion to CSV completed."
	return ()
