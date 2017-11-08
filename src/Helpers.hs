--Where formulas for finding classes to begin with are located
module Helpers where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B (readFile)
import           Data.List                  (find, isInfixOf)
import           Data.Maybe
import           Datatypes

--Function to help find a specific course
findCourse :: IO FullCourse
findCourse = do
  inp <- getLine
  results <- filter (isInfixOf inp . courseTitle) <$> allClasses :: IO [FullCourse]
  case length results of
    0 -> print "No results" >> return (FullCourse "" "" [])
    1 -> print (courseTitle $ head results) >> return (head results)
    _ -> do
          print $ courseTitle <$> results
          inp' <- getLine
          return $ head $ filter (isInfixOf inp . courseTitle) results

--Moved this to the Helper module
allClasses :: IO [FullCourse]
allClasses =
  fromMaybe [] . decode
  <$> B.readFile "src\\MutatedClasses.json"
