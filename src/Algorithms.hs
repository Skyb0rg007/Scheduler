

module Algorithms where

import           Data.List  (sort)
import           Data.Maybe
import           Datatypes

--Returns all of the schedules for a class on a given day
concatClass :: FullCourse -> DayOfWeek -> [Day]
concatClass c d = filter (\x -> day x == d) unfiltered
  where sects      = sections c
        unfiltered = concatMap schedules sects

--Returns whether two classes conflict on a given day
mustBeOne :: FullCourse -> FullCourse -> DayOfWeek -> Bool
mustBeOne c1 c2 d = do
  let days1   = concatClass c1 d :: [Day]
      days2   = concatClass c2 d :: [Day]
      combos  = [(x,y) | x <- days1, y <- days2] :: [(Day, Day)]
      matches = uncurry (/=) <$> combos          :: [Bool]
      -- zipped  = zip combos matches               :: [((Day,Day),Bool)]
      -- works   = mapMaybe (\(x,y) -> if y then Just x else Nothing)
      --                    zipped
  or matches

classesDontConflict :: FullCourse -> FullCourse -> Bool
classesDontConflict c1 c2 | c1 == c2 = False
                          | otherwise = True
