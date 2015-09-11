module Parse.Log where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B8
import Data.Time (ZonedTime)

import Parse.DateTime
import Parse.HTTP
import Types


logEntry :: IP -> Maybe B8.ByteString -> Maybe B8.ByteString -> ZonedTime -> (Maybe HTTPMethod, URL, Maybe Protocol, ProtocolVersion) -> Maybe Int -> Int -> LogEntry
logEntry a b c d (e, f, g, h) i j = LogEntry a b c d e f g h i j


parseLogLine :: Parser LogEntry
parseLogLine = fmap logEntry
		parseIP
	<*> (space *> parseUserident)
	<*> (space *> parseUserident)
	<*> (space *> parseTimeAndDate)
	<*> (space *> parseRequestLine)
	<*> (space *> parseHTTPStatus)
	<*> (space *> parseByteSize)
