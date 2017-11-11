module Main where

import           Algorithms (total)
import           Datatypes
import           Helpers
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  courses <- findCourses
  let scheds = total courses
  filtered <- repeatFilter $ return scheds
  print filtered

repeatFilter :: IO [[Section]] -> IO [[Section]]
repeatFilter ss = do
  x <- ss
  putStrLn $ "You currently have " ++ show (length x) ++ " schedules.\n\
    \Would you like to filter out more choices?"
  r <- getLine
  case r of
    "y" -> repeatFilter $ filterSchedules x
    "n" -> return x
