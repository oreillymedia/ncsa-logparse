module CommandLineArgs (CommandLineOpts, logPath, opts, verbose) where

import Data.Version (showVersion)
import Options.Applicative
import Text.Format (format)

import Paths_ncsa_logparse (version)


data CommandLineOpts = CommandLineOpts
	{ logPath :: FilePath
	, verbose :: Bool }
	deriving Show


parseOpts :: Parser CommandLineOpts
parseOpts = CommandLineOpts
	<$> argument str
	    (  metavar "PATH_TO_LOGFILE"
	    <> help "Path to the NCSA-compatible log file." )
 	<*> switch ( long "verbose" <> help "Show output explaining what the command is doing as it runs." <> hidden )


 -- | A helper function that gets the project version from the cabal file.
versionString :: String
versionString = showVersion version


-- | A command line arguments parser for the ncsa-logparse executable.
opts :: ParserInfo CommandLineOpts
opts = info (helper <*> parseOpts)
	   (  fullDesc
	   <> progDesc "Parse a log file"
	   <> header description )
	where description = format "Parse an NCSA-compatible log file. Version {0}" [versionString]