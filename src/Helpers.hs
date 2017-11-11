--Where formulas for finding and filtering classes are located
module Helpers (allClasses,
                findCourse, findCourses,
                filterSchedules, filterTeach, filterEarly, filterLate)
                where

import           Control.Monad              (replicateM)
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Lazy.Char8 as B (readFile)
import           Data.List                  (isInfixOf, nub)
import           Data.Maybe                 (fromJust)
import           Datatypes                  (Day (..), DayOfWeek,
                                             FullCourse (courseTitle),
                                             Section (instructor, schedules))

--Makes a list out of your classes
findCourses :: IO [FullCourse]
findCourses = do
  cs <- allClasses
  putStrLn "How many classes do you want to choose?"
  n <- read <$> getLine :: IO Int
  replicateM n (findCourse cs)

--Recursively narrows down searches until you find your desired class
findCourse :: [FullCourse] -> IO FullCourse
findCourse classes = do
  putStr $ "There are " ++ (show . length) classes ++ " classes.\nEnter a filter: "
  results <- findCourse' classes
  case results of
    Nothing -> putStrLn "No results!"
                >> findCourse classes
    Just cs@(a:b:c) -> print (courseTitle <$> cs)
                >> findCourse cs
    Just c -> putStrLn ("Class selected: " ++ courseTitle (head c))
                >> return (head c)

--Function to help find a specific course
findCourse' :: [FullCourse] -> IO (Maybe [FullCourse])
findCourse' cs = do
  inp <- getLine
  let filtered = modify inp cs
  case length filtered of
    0 -> return Nothing
    _ -> return $ Just filtered

--The filter function, uses courseTitle
modify :: String -> [FullCourse] -> [FullCourse]
modify s = filter (\c -> s `isInfixOf` courseTitle c)

--After figuring out your final schedules, can throw out finished schedules
--based on teacher or time of classes
filterSchedules :: [[Section]] -> IO [[Section]]
filterSchedules ss = do
  let l = length ss
  putStrLn $ "You have " ++ show l ++ " schedules.\n\
    \Would you like to filter by: \n\
    \1. instructor\n\
    \2. start time\n\
    \3. end time"
  i <- getLine
  case i of
    "1" -> filterTeach ss
    "2" -> filterEarly ss
    "3" -> filterLate ss
    _   -> error "Invalid Choice"

filterTeach :: [[Section]] -> IO [[Section]]
filterTeach ss = do
  putStrLn "Enter the instructor's name.\n\
    \Try to state the full name if possible."
  n <- getLine
  print $ nub $ filter (isInfixOf n) (instructor <$> concat ss)
  putStrLn "Are these the teachers you want to filter out?"
  r <- getLine
  case r of
    "y" -> return $ filter (not . hasTeach n) ss
    "n" -> return ss
  where hasTeach :: String -> [Section] -> Bool
        hasTeach n ss = or $ (isInfixOf n . instructor) <$> ss

filterEarly :: [[Section]] -> IO [[Section]]
filterEarly ss = do
  putStrLn "Enter the earliest time (in hours) for a class to start\n\
    \Ex. 8 -> 8 AM, 13 -> 1 PM"
  strt <- (*60) . read <$> getLine :: IO Int
  let filtered = filter (not . isEarly strt) ss
  filterConfirm ss filtered
  where isEarly :: Int -> [Section] -> Bool
        isEarly t ss = or $ (<t) <$> (start <$> concatMap schedules ss)

filterLate :: [[Section]] -> IO [[Section]]
filterLate ss = do
  putStrLn "Enter the latest time (in hours) for a class to end\n\
    \Ex. 20 -> 8 PM, 18 -> 6 PM"
  e <- (*60) . read <$> getLine :: IO Int
  let filtered = filter (not. isLate e) ss
  filterConfirm ss filtered
  where isLate :: Int -> [Section] -> Bool
        isLate t ss = or $ (>t) <$> (end <$> concatMap schedules ss)

filterConfirm :: [[Section]] -> [[Section]] -> IO [[Section]]
filterConfirm ss filtered = do
  putStrLn $ "There are " ++ show (length ss - length filtered) ++
    " classes excluded.\n\
    \Are you sure you want to filter them?"
  r <- getLine
  case r of
    "y" -> return filtered
    "n" -> return ss

--All of the courses as the Haskell datatype defined in Datatypes.hs
allClasses :: IO [FullCourse]
allClasses =
  fromJust . decode
  <$> B.readFile "src\\MutatedClasses.json"
