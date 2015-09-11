module Parse.HTTP where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B8

import Types
import Parse.Util


parseIP :: Parser IP
parseIP = fmap IP
	    (decimal <* period)
	<*> (decimal <* period)
	<*> (decimal <* period)
	<*> decimal


parseHTTPMethod :: Parser (Maybe HTTPMethod)
parseHTTPMethod =
	    (stringCI "GET" *> return (Just Get))
	<|> (stringCI "POST" *> return (Just Post))
	<|> (stringCI "PUT" *> return (Just Put))
	<|> (stringCI "DELETE" *> return (Just Delete))
	<|> (stringCI "OPTIONS" *> return (Just Options))
	<|> (stringCI "HEAD" *> return (Just Head))
	<|> (stringCI "TRACE" *> return (Just Trace))
	<|> (stringCI "CONNECT" *> return (Just Connect))
	<|> return Nothing


parseHTTPStatus :: Parser (Maybe Int)
parseHTTPStatus = validate <$> decimal
	where
		validate d = if (d >= 200 && d < 506) then Just d else Nothing


parseProtocol :: Parser (Maybe Protocol)
parseProtocol =
	    (stringCI "HTTPS" *> return (Just HTTPS))
	<|> (stringCI "HTTP" *> return (Just HTTP))
	<|> (stringCI "FTP" *> return (Just FTP))
	<|> return Nothing


parseProtocolVersion :: Parser ProtocolVersion
parseProtocolVersion = protocolVersion <$> (liftA2 (,) decimal (char '.' *> decimal))
	where protocolVersion (mA, mN) = ProtocolVersion mA mN


parseUserident :: Parser (Maybe B8.ByteString)
parseUserident = do
	ident <- takeTill (== ' ')
	return $ if ((B8.length ident) == 1) && (B8.head ident) == '-' then Nothing else Just ident


parseByteSize :: Parser Int
parseByteSize = decimal


parseURL :: Parser URL
parseURL = takeTill (== ' ')


-- |Parse the request line section of the log, e.g. "GET /apache_pb.gif HTTP/1.0"
parseRequestLine :: Parser (Maybe HTTPMethod, URL, Maybe Protocol, ProtocolVersion)
parseRequestLine = fmap (,,,)
	    (quote *> parseHTTPMethod)
	<*> (space *> parseURL)
	<*> (space *> parseProtocol)
	<*> (slash *> parseProtocolVersion <* quote)
