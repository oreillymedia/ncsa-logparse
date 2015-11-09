module Parse.Util ( parseDigits, slash, quote, period ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Numeric (readDec)



-- | Parse a given number of numeric characters into a Numeric representation (i.e. Int or Integer)
parseDigits :: (Read a, Num a, Eq a) => Int -> Parser a
parseDigits n = (fst . head . readDec) <$> count n digit


-- |Parse a slash character.
slash :: Parser Char
slash = char '/'


-- |Parse a quote character.
quote :: Parser Char
quote = char '"'


-- |Parse a period character.
period :: Parser Char
period = char '.'