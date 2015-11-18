module Parse.Log where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Either (rights)
import Data.Time (ZonedTime)
import Web.UAParser

import Parse.DateTime
import Parse.HTTP
import Parse.Util
import Types


{--

    Parser for the NCSA extended and common log formats (defined here: https://httpd.apache.org/docs/trunk/logs.html)

--}


parseFileLines :: (Parser LogEntry) -> [BL.ByteString] -> Log
parseFileLines parser rawLogLines = rights $ map (parseFileLine parser) rawLogLines


parseFileLine :: (Parser LogEntry) -> BL.ByteString -> Either String LogEntry
parseFileLine p logFileLine = parseOnly p ln
    where
        ln = (B.concat . BL.toChunks) logFileLine


-- | Parse and expand a given user agent string into its constituent browser and platform data.
expandUA :: Maybe B8.ByteString -> (Maybe UAResult, Maybe OSResult)
expandUA ua = case ua of
    Nothing    -> (Nothing, Nothing)
    Just rawUA -> (parseUA rawUA, parseOS rawUA)


parseAsCommonLogLine :: Parser LogEntry
parseAsCommonLogLine = fmap LogEntry
        parseIP
    <*> (space *> parseUserident)
    <*> (space *> parseUserident)
    <*> (space *> parseTimeAndDate)
    <*> (space *> quote *> parseHTTPMethod)
    <*> (space *> parseURL)
    <*> (space *> parseProtocol)
    <*> (slash *> parseProtocolVersion <* quote)
    <*> (space *> parseHTTPStatus)
    <*> (space *> parseByteSize)
    <*> return Nothing
    <*> return Nothing
    <*> return Nothing
    <*> return Nothing


-- 216.74.39.38 - - [12/Jan/2015:20:37:55 +0000] 
--"GET index.htm HTTP/1.0" 200 215 "http://www.example.com/start.html" "Mozilla/4.08 [en] (Win98; I ;Nav)"

parseAsExtendedLogLine :: Parser LogEntry
parseAsExtendedLogLine = fmap extendedLogEntry
        parseIP
    <*> (space *> parseUserident)
    <*> (space *> parseUserident)
    <*> (space *> parseTimeAndDate)
    <*> (space *> quote *> parseHTTPMethod)
    <*> (space *> parseURL)
    <*> (space *> parseProtocol)
    <*> (slash *> parseProtocolVersion <* quote)
    <*> (space *> parseHTTPStatus)
    <*> (space *> parseByteSize)
    <*> (space *> parseQuotedValue)
    <*> (space *> parseQuotedValue)
    where
        extendedLogEntry a b c d e f g h i j k l =
            let (ua, os) = expandUA l
            in LogEntry {
                ip=a,
                identity=b,
                userid=c,
                timestamp=d,
                method=e,
                url=f,
                proto=g,
                protoVer=h,
                status=i,
                byteSize=j,
                referrer=k,
                userAgent=l,
                browser=ua,
                platform=os
            }

