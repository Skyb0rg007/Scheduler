--"Where the real works gets done"
--This is where the formulas for getting schedules are located
module Algorithms where

import           Datatypes

--Whether two sections conflict
conflict :: Section -> Section -> Bool
conflict s1 s2 = and $ (\(x,y) -> x /= y || day x /= day y) <$> zipped
  where zipped = [(x,y) | x <- schedules s1, y <- schedules s2]

--Whether a section conflicts with a list of sections
--(provided the list of sections don't conflict with each other)
conflicts :: [Section] -> Section -> Bool
conflicts ss s = and $ conflict s <$> ss

--Culmination of everything that needs to be done
--   To Choose From | Already chosen | Final working combos
total :: [FullCourse] -> [Section] -> [[Section]]
total [] d      = [d]
  --If no more to choose from, then you're done
total (x:xs) [] = concat $ total xs . return <$> sections x
  --How to start out if nothing is chosen yet
  --(call total for each Section in the first class)
total (x:xs) used = do
  let sects = sections x :: [Section]
      works = filter (conflicts used) sects :: [Section]
  concat $ (\n -> total xs (n:used)) <$> works
  --Find every section that fits every pre-chosen section
  --Call total on each, adding the working section to 'used'
