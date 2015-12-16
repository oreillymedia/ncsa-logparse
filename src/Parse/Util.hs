module Parse.Util (parseDigits, parseInt, parseInteger, slash, quote, period) where

import Prelude hiding (take)

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Numeric (readDec)



-- | Parse a given number of numeric characters into a Numeric representation (i.e. Int or Integer). Performance
-- of this parser is not that good. Use parseInt and parseInteger when possible.
parseDigits :: (Read a, Num a, Eq a) => Int -> Parser a
parseDigits n = (fst . head . readDec) <$> count n digit


unwrapIntegral :: Integral a => Maybe (a, B.ByteString) -> Maybe a
unwrapIntegral Nothing = Nothing
unwrapIntegral (Just (num, extra)) = case B8.length extra == 0 of
    True  -> Just num
    False -> Nothing


-- | Parse the given number of numeric characters and convert to an Int. If the n parsed chars contain
-- | a non-numeric character, parser will fail.
parseInt :: Int -> Parser Int
parseInt size = do
    input <- (unwrapIntegral . B8.readInt) <$> take size
    case input of
        Nothing -> fail "Characters parsed contained a non-numeric char."
        (Just n) -> return n


-- | Parse the given number of numeric characters and convert to an Integer. If the n parsed chars contain
-- | a non-numeric character, parser will fail.
parseInteger :: Int -> Parser Integer
parseInteger size = do
    input <- (unwrapIntegral . B8.readInteger) <$> take size
    case input of
        Nothing -> fail "Characters parsed contained a non-numeric char."
        (Just n) -> return n


-- |Parse a slash character.
slash :: Parser Char
slash = char '/'


-- |Parse a quote character.
quote :: Parser Char
quote = char '"'


-- |Parse a period character.
period :: Parser Char
period = char '.'