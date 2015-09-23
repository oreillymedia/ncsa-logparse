module Parse.Log where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.Time (ZonedTime)
import Web.UAParser

import CommandLineArgs
import Parse.DateTime
import Parse.HTTP
import Types


{--

	Parser for the NCSA extended and common log formats (defined here: https://httpd.apache.org/docs/trunk/logs.html)

--}



parseFileLines :: (Parser LogEntry) -> [BL.ByteString] -> Log
parseFileLines p rawLogLines = rights $ map  (parseFileLine p) rawLogLines


parseFileLine :: (Parser LogEntry) -> BL.ByteString -> Either String LogEntry
parseFileLine p logFileLine = ln >>= parseOnly p
    where
        ln = (Right . B.concat . BL.toChunks) logFileLine


expandUA :: Maybe B8.ByteString -> (Maybe UAResult, Maybe OSResult)
expandUA ua = case ua of
	Nothing    -> (Nothing, Nothing)
	Just rawUA -> (parseUA rawUA, parseOS rawUA)



commonLogEntry :: IP -> Maybe B8.ByteString -> Maybe B8.ByteString -> ZonedTime -> (Maybe HTTPMethod, URL, Maybe Protocol, ProtocolVersion) -> Maybe Int -> Int -> LogEntry
commonLogEntry a b c d (e, f, g, h) i j = LogEntry Common a b c d e f g h i j Nothing Nothing Nothing Nothing


parseAsCommonLogLine :: Parser LogEntry
parseAsCommonLogLine = fmap commonLogEntry
		parseIP
	<*> (space *> parseUserident)
	<*> (space *> parseUserident)
	<*> (space *> parseTimeAndDate)
	<*> (space *> parseRequestLine)
	<*> (space *> parseHTTPStatus)
	<*> (space *> parseByteSize)


extendedLogEntry :: IP -> Maybe B8.ByteString -> Maybe B8.ByteString -> ZonedTime -> (Maybe HTTPMethod, URL, Maybe Protocol, ProtocolVersion) -> Maybe Int -> Int -> Maybe B8.ByteString -> Maybe B8.ByteString -> LogEntry
extendedLogEntry a b c d (e, f, g, h) i j k l = LogEntry Extended a b c d e f g h i j k l ua os
	where (ua, os) = expandUA l


parseAsExtendedLogLine :: Parser LogEntry
parseAsExtendedLogLine = fmap extendedLogEntry
		parseIP
	<*> (space *> parseUserident)
	<*> (space *> parseUserident)
	<*> (space *> parseTimeAndDate)
	<*> (space *> parseRequestLine)
	<*> (space *> parseHTTPStatus)
	<*> (space *> parseByteSize)
	<*> (space *> parseReferrer)
	<*> (space *> parseUserAgent)


chooseParsingFunction :: CommandLineOpts -> (Parser LogEntry)
chooseParsingFunction args = case parseAsCommon args of
	True  -> parseAsCommonLogLine
	False -> parseAsExtendedLogLine

