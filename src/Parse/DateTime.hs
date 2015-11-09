module Parse.DateTime ( monthNameToNumber, parseTimeAndDate, parseTZ, parseMonthName ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.Time
import System.Locale (defaultTimeLocale, months)

import Parse.Util


-- |Parse a date/time string into a LocalTime object from a string like "[10/Oct/2000:13:55:36 -0700]"
parseTimeAndDate :: Parser ZonedTime
parseTimeAndDate = do
	_  <- char '['
	d  <- parseDigits 2
	_  <- slash
	mm <- parseMonthName
	_  <- slash
	y  <- parseDigits 4
	_  <- char ':'
	h  <- parseDigits 2
	_  <- char ':'
	m  <- parseDigits 2
	_  <- char ':'
	s  <- parseDigits 2
	_  <- char ' '
	tz <- parseTZ
	_  <- char ']'
	let localTime = LocalTime {
				localDay = fromGregorian y mm d,
				localTimeOfDay = TimeOfDay h m s

			}
	return ZonedTime {
				zonedTimeToLocalTime = localTime,
				zonedTimeZone = tz
	}


-- |Parse a ISO-8601 timezone string (e.g. -0700, +05, 09) into a TimeZone object
-- FIXME: Add some sanity checking to make sure the offset isn't too big to be real: (-12:00 ≤ offset ≤ +14:00)
parseTZ :: Parser TimeZone
parseTZ = minutesToTimeZone <$> signed parseTZDigits
	where parseTZDigits = liftA2 (+) ((* 60) <$> parseDigits 2) (parseDigits 2 <|> return 0)


-- | A map from three-letter month names (case insensitive) to their numeric representation (e.g. "Jun" -> 6)
monthMap :: Map.Map String Int
monthMap = let monthNames = map ((map toLower) . snd) (months defaultTimeLocale)
	       in Map.fromList $ zip monthNames [1..]


-- | Convert a three-letter month name (case-insensitive) to its numeric representation.
monthNameToNumber :: String -> Maybe Int
monthNameToNumber monthName = Map.lookup normalizedMonthName monthMap
	where normalizedMonthName = map toLower monthName


-- |Parse a three-letter month name into its numeric representation.
parseMonthName :: Parser Int
parseMonthName = monthNameToNumber <$> count 3 letter_ascii >>= checkMonth
	where checkMonth m = maybe (fail "Invalid month name.") return m
