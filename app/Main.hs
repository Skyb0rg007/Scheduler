module Main where

import           Algorithms
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B (pack, readFile)
import           Data.Maybe
import           Datatypes
import           Lib

main :: IO ()
main = do
  c <- B.readFile "src\\MutatedClasses.json"
  let m = decode c :: Maybe [FullCourse]
      x = fromMaybe [] m
      y = take 10 (drop 100 x)
  print y

allClasses :: IO [FullCourse]
allClasses =
  fromMaybe [] . decode
  <$> B.readFile "src\\MutatedClasses.json"
