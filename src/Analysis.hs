module Analysis where

import qualified Data.Text as T
import Web.UAParser

import Types
import Utils (catMaybes)



-- | Return the total number of bytes sent by the server in a log file.
sumBytes :: Log -> Int
sumBytes log = sum $ map byteSize log


countNotFoundResponses :: Log -> Int
countNotFoundResponses log = length $ (filter (== 404)) $ catMaybes $ map status log


-- | Return a list of distinct OS/Platform families in a log file.
uniqueOSFamilies :: Log -> [String]
uniqueOSFamilies log = map T.unpack allPlatforms
	where
		allPlatforms = nub $ map osrFamily $ catMaybes $ map platform log


-- | Return a list of distinct Browsers in a log file.
uniqueBrowsers :: Log -> [String]
uniqueBrowsers log = map T.unpack allBrowsers
	where
		allBrowsers = nub $ map uarFamily $ catMaybes $ map browser log
