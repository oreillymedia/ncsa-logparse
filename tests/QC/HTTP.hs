module QC.HTTP (tests) where

import Data.Either
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Parse.HTTP
import Types


tests :: [Test]
tests = [
	  testProperty "propHttpParse" propHttpParse
	, testProperty "propHTTPStatus" propHTTPStatus
	, testProperty "propProtocol" propProtocol
	, testProperty "propProtocolVersion" propProtocolVersion
	, testProperty "propIPAddress" propIPAddress
  ]


testParser :: Eq b => [B.ByteString] -> [b] -> Parser b -> Bool
testParser inputs outputs parser = (rights $ map (parseOnly parser) inputs) == outputs


propHttpParse :: Bool
propHttpParse =
                testParser
                ["GET", "POST", "connect", "Trace", "FOO"]
                [Just Get, Just Post, Just Connect, Just Trace, Nothing]
                parseHTTPMethod


propHTTPStatus :: Bool
propHTTPStatus =
                testParser
                ["200", "301", "404", "502", "199", "506", "1", "10000", "A"]
                [Just 200, Just 301, Just 404, Just 502, Nothing, Nothing, Nothing, Nothing]
                parseHTTPStatus


propProtocol :: Bool
propProtocol =
                testParser
                ["HTTP", "HTTPS", "FTP", "GOPHER", ""]
                [Just HTTP, Just HTTPS, Just FTP, Nothing, Nothing]
                parseProtocol


propProtocolVersion :: Bool
propProtocolVersion =
                testParser
                ["0.9", "1.0", "5.5", "10.23", "1.A"]
                [ProtocolVersion 0 9, ProtocolVersion 1 0, ProtocolVersion 5 5, ProtocolVersion 10 23]
                parseProtocolVersion


propIPAddress :: Bool
propIPAddress =
                testParser
                ["A.B.C.D", "0.0.0", "192.168.1.1", "55.12.1.18", "127.0.0.1"]
                [IP 192 168 1 1, IP 55 12 1 18, IP 127 0 0 1]
                parseIP
