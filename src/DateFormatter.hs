{-# LANGUAGE OverloadedStrings #-}

module DateFormatter (formatZonedTime) where

import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Internal
import Data.Time (ZonedTime)
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Word
import Foreign.Ptr
import Foreign.Storable

{--

    Fast formatting of ZonedTime in ISO-8601 style (e.g. "2015-12-01 22:02:45 +0430")

    The formatTime function in Data.Time.Format is know to be very slow. This version, adapted
    from the `http-date` package is considerably faster.

--}


-- | Formatting a ZonedTime in ISO-8601 style: "2015-12-01 22:02:45 +0430"
formatZonedTime :: ZonedTime -> B8.ByteString
formatZonedTime zt = B8.concat [ltS, " ", tzS]
    where
        ltS = (formatLocalTime . zonedTimeToLocalTime) zt
        tzS = (B8.pack . timeZoneOffsetString . zonedTimeZone) zt


-- | Formatting a LocalTime in ISO-8601 style: "2015-12-01 22:02:45"
formatLocalTime :: LocalTime -> B8.ByteString
formatLocalTime lt =
    unsafeCreate 19 $ \ptr -> do
        int4 ptr (fromInteger y)
        poke (ptr `plusPtr` 4) hyphen
        int2 (ptr `plusPtr` 5) m
        poke (ptr `plusPtr` 7) hyphen
        int2 (ptr `plusPtr` 8) d
        poke (ptr `plusPtr` 10) spc
        int2 (ptr `plusPtr` 11) h
        poke (ptr `plusPtr` 13) colon
        int2 (ptr `plusPtr` 14) mm
        poke (ptr `plusPtr` 16) colon
        int2 (ptr `plusPtr` 17) s
    where
        (y, m, d) = (toGregorian . localDay) lt
        time = localTimeOfDay lt
        h = todHour time
        mm = todMin time
        -- Seconds are represented as a fixed-precision type. Convert
        -- to an integer by rounding up from microseconds.
        s = ceiling $ toRational $ todSec time


int2 :: Ptr Word8 -> Int -> IO ()
int2 ptr n
  | n < 10 = do
      poke ptr zero
      poke (ptr `plusPtr` 1) (i2w8 n)
  | otherwise = do
      poke ptr               (i2w8 (n `quot` 10))
      poke (ptr `plusPtr` 1) (i2w8 (n `rem` 10))

int4 :: Ptr Word8 -> Int -> IO ()
int4 ptr n0 = do
    let (n1,x1) = n0 `quotRem` 10
        (n2,x2) = n1 `quotRem` 10
        (x4,x3) = n2 `quotRem` 10
    poke ptr               (i2w8 x4)
    poke (ptr `plusPtr` 1) (i2w8 x3)
    poke (ptr `plusPtr` 2) (i2w8 x2)
    poke (ptr `plusPtr` 3) (i2w8 x1)


i2w8 :: Int -> Word8
i2w8 n = fromIntegral n + zero


spc :: Word8
spc = 32


colon :: Word8
colon = 58


zero :: Word8
zero = 48


hyphen :: Word8
hyphen = 45