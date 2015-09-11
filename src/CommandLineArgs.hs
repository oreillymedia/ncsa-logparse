module CommandLineArgs (CommandLineOpts, logPath, opts, verbose) where

import Data.Version (showVersion)
import Options.Applicative
import Text.Format (format)

import Paths_ncsa_logparse (version)


data LogFormatType =  NCSACommon | NCSAExtended deriving (Eq)


instance Show LogFormatType where
	show NCSACommon   = "common"
	show NCSAExtended = "extended"


data CommandLineOpts = CommandLineOpts
	{ logPath :: FilePath
	, logFormat :: LogFormatType
	, verbose :: Bool }
	deriving Show


 -- | A helper function that gets the project version from the cabal file.
versionString :: String
versionString = showVersion version


-- |Given an input string that should contain an a valid format specifier, return either an error message or LogFormatType
logFormatReader :: String -> Either String LogFormatType
logFormatReader "common"   = Right NCSACommon
logFormatReader "extended" = Right NCSAExtended
logFormatReader input      = Left ( "Invalid format: " ++ input )


parseOpts :: Parser CommandLineOpts
parseOpts = CommandLineOpts
	<$> argument str
	    (  metavar "PATH_TO_LOGFILE"
	    <> help "Path to the NCSA-compatible log file." )
 	<*> option (eitherReader logFormatReader)
 	    (  long "format"
 	    <> short 'f'
 	    <> value NCSACommon
 	    <> showDefault
 	    <> metavar "FORMAT"
 	    <> help "Format of logfile; must be a value of \"common\" or \"extended\"." )
 	<*> switch ( long "verbose" <> help "Show output explaining what the command is doing as it runs." <> hidden )


-- | A command line arguments parser for the ncsa-logparse executable.
opts :: ParserInfo CommandLineOpts
opts = info (helper <*> parseOpts)
	   (  fullDesc
	   <> progDesc "Parse a log file"
	   <> header description )
	where description = format "Parse an NCSA-compatible log file. Version {0}" [versionString]
