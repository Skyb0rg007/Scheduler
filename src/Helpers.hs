--Where formulas for finding classes to begin with are located
module Helpers (findCourse, findCourses, allClasses) where

import           Data.Aeson                 (decode)
import qualified Data.ByteString.Lazy.Char8 as B (readFile)
import           Data.List                  (find, isInfixOf)
import           Data.Maybe                 (fromJust)
import           Datatypes                  (FullCourse (courseTitle),
                                             emptyClass)

--Function to help find a specific course
findCourse' :: IO FullCourse
findCourse' = do
  inp <- getLine
  results <- filter (isInfixOf inp . courseTitle) <$> allClasses :: IO [FullCourse]
  case length results of
    0 -> print "No results" >> return emptyClass
    1 -> print (courseTitle $ head results) >> return (head results)
    _ -> do
          print $ courseTitle <$> results
          inp' <- getLine
          let results' = filter (isInfixOf inp . courseTitle) results
          case length results' of
            0 -> print "Too limiting: no results" >> return emptyClass
            1 -> return $ head results'
            _ -> print "More than one result! Using first listed"
                  >> return (head results')

findCourses :: IO [FullCourse]
findCourses = do
  putStrLn "Enter a phrase to search for a class"
  inp <- getLine
  classes <- findCourse inp
  case length classes of
    0 -> putStrLn "Too specific, no results."
          >> findCourses
    1 -> do
      print $ courseTitle $ head classes
      putStrLn "Is this the right choice? (y/n)"
      ans <- getChar
      case ans of
        'y' -> putStrLn "Another? (y/n)" >> getChar >>= \inp2 ->
              if inp2 == 'y'
                then (\xs -> head classes:xs) <$> findCourses
                else return classes
        'n' -> findCourses
    _ -> do
      putStrLn "More than one result:"
      putStrLn $ unlines $ courseTitle <$> classes
      inp3 <- getLine
      let classes' = filter (isInfixOf inp3 . courseTitle) classes
      print $ unlines $ courseTitle <$> classes'
      putStrLn "Choose the number of the result"
      inp4 <- getLine
      putStrLn "more?"
      inp5 <- getChar
      if inp5 == 'y'
        then ((classes' !! read inp4):) <$> findCourses
        else return . return $ classes' !! read inp4



findCourse :: String -> IO [FullCourse]
findCourse inp = filter (isInfixOf inp . courseTitle) <$> allClasses

--Moved this to the Helper module
allClasses :: IO [FullCourse]
allClasses =
  fromJust . decode
  <$> B.readFile "src\\MutatedClasses.json"
