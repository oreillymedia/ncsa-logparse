module Types where


import qualified Data.ByteString.Char8 as BC (ByteString)
import Data.Time (ZonedTime)
import Data.Word (Word8)
import Web.UAParser (OSResult, UAResult)



-- |IP Address
data IP = IP Word8 Word8 Word8 Word8 deriving (Eq)


instance Show IP where
	show (IP b1 b2 b3 b4)
						= show b1 ++ "."
					   ++ show b2 ++ "."
					   ++ show b3 ++ "."
					   ++ show b4


-- |Type synonym
type URL = BC.ByteString


-- |Possible HTTP methods
data HTTPMethod = Get | Post | Put | Delete | Options | Head | Trace | Connect deriving (Show, Eq)


-- |Possible protocols
data Protocol = HTTP | HTTPS | FTP deriving (Show, Eq)


-- |Protocol version representation
data ProtocolVersion = ProtocolVersion {
	majorVersion :: Int,
	minorVersion :: Int
} deriving (Eq)


instance Show ProtocolVersion where
	show (ProtocolVersion major minor)
									 = show major ++ "."
									++ show minor



-- |A single log line from an NCSA Common or Extended-formatted log
data LogEntry = LogEntry {
	ip 		   :: IP,
	identity   :: Maybe BC.ByteString,
	userid     :: Maybe BC.ByteString,
	timestamp  :: ZonedTime,
	method     :: Maybe HTTPMethod,
	url        :: URL,
	proto      :: Maybe Protocol,
	protoVer   :: ProtocolVersion,
	status     :: Maybe Int,
	byteSize   :: Int,
	referrer   :: Maybe URL,
	userAgent  :: Maybe BC.ByteString,
	browser    :: Maybe UAResult,
	platform   :: Maybe OSResult
} deriving (Show)



-- |A parsed log file (i.e. a list of line records)
type Log = [LogEntry]



