module Parse.Log where

import Prelude hiding (map)

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.List.Stream (map)
import Data.Time (ZonedTime)
import Web.UAParser

import Parse.DateTime
import Parse.HTTP
import Utils (rights)
import Types


{--

	Parser for the NCSA extended and common log formats (defined here: https://httpd.apache.org/docs/trunk/logs.html)

--}



parseFileLines :: (Parser LogEntry) -> [BL.ByteString] -> Log
parseFileLines p rawLogLines = rights $ map (parseFileLine p) rawLogLines


parseFileLine :: (Parser LogEntry) -> BL.ByteString -> Either String LogEntry
parseFileLine p logFileLine = ln >>= parseOnly p
    where
        ln = (Right . B.concat . BL.toChunks) logFileLine


-- | Parse and expand a given user agent string into its consitutent browser and platform data.
expandUA :: Maybe B8.ByteString -> (Maybe UAResult, Maybe OSResult)
expandUA ua = case ua of
	Nothing    -> (Nothing, Nothing)
	Just rawUA -> (parseUA rawUA, parseOS rawUA)



parseAsCommonLogLine :: Parser LogEntry
parseAsCommonLogLine = fmap commonLogEntry
		parseIP
	<*> (space *> parseUserident)
	<*> (space *> parseUserident)
	<*> (space *> parseTimeAndDate)
	<*> (space *> parseRequestLine)
	<*> (space *> parseHTTPStatus)
	<*> (space *> parseByteSize)
	where
		commonLogEntry :: IP -> Maybe B8.ByteString -> Maybe B8.ByteString -> ZonedTime -> (Maybe HTTPMethod, URL, Maybe Protocol, ProtocolVersion) -> Maybe Int -> Int -> LogEntry
		commonLogEntry a b c d (e, f, g, h) i j = LogEntry a b c d e f g h i j Nothing Nothing Nothing Nothing



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
	where
		extendedLogEntry :: IP -> Maybe B8.ByteString -> Maybe B8.ByteString -> ZonedTime -> (Maybe HTTPMethod, URL, Maybe Protocol, ProtocolVersion) -> Maybe Int -> Int -> Maybe B8.ByteString -> Maybe B8.ByteString -> LogEntry
		extendedLogEntry a b c d (e, f, g, h) i j k l =
			let (ua, os) = expandUA l
			in LogEntry a b c d e f g h i j k l ua os


