{-# LANGUAGE DeriveGeneric #-}

module Csv where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Csv
import GHC.Generics

import Types
import Utils (formatZonedTime)


data CSVLine = CSVLine {
	csvIP 		  :: BC.ByteString,
	csvIdentity   :: Maybe BC.ByteString,
	csvUserid     :: Maybe BC.ByteString,
	csvTimestamp  :: BC.ByteString,
	csvMethod     :: Maybe BC.ByteString,
	csvUrl        :: URL,
	csvProto      :: Maybe BC.ByteString,
	csvProtoVer   :: BC.ByteString,
	csvStatus     :: Maybe Int,
	csvByteSize   :: Int,
	csvReferrer   :: Maybe URL,
	csvUserAgent  :: Maybe BC.ByteString
} deriving (Show, Generic)

instance ToRecord CSVLine


maybeShowByteString :: Show a => Maybe a -> Maybe BC.ByteString
maybeShowByteString (Just m) = Just $ (BC.pack . show) m
maybeShowByteString Nothing = Nothing


toCsvLine :: LogEntry -> CSVLine
toCsvLine lg = CSVLine _ip _ident _userid _ts _method _url _proto _protoV _status _bytes _ref _ua
	where
		_ip     = (BC.pack . show . ip) lg
		_ident  = identity lg
		_userid = userid lg
		_ts     = (BC.pack . formatZonedTime . timestamp) lg
		_method = maybeShowByteString $ method lg
		_url    = url lg
		_proto  = maybeShowByteString $ proto lg
		_protoV = (BC.pack . show . protoVer) lg
		_status = status lg
		_bytes  = byteSize lg
		_ref    = referrer lg
		_ua     = userAgent lg


toCSV::Log -> BL.ByteString
toCSV = encode . (map toCsvLine)

