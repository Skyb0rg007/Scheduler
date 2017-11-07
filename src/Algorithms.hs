

module Algorithms where

--import           Data.List (sort)
--import           Data.Maybe
import           Datatypes

--turns a course into just the important bits
concatClasses :: FullCourse -> [[Day]]
concatClasses c = schedules <$> sections c

--returns whether two 'sections' conflict
mustBeOne :: [Day] -> [Day] -> [(Day,Day)]
mustBeOne ds1 ds2 = do
  let zipped  = [(x,y) | x <- ds1, y <- ds2]
                :: [(Day,Day)] --Create all combinations
      works   = filter (\(x,y) -> (x /= y || day x /= day y)) zipped
                :: [(Day,Day)] -- Get rid of combos that conflict
  works
--
-- --returns the
-- theOne :: [Day] -> [Day] -> [[(Day,Day)]]
-- theOne
