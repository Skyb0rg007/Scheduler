{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Dataypes2 where

import           Data.Aeson
import qualified Data.Map     as M
import           GHC.Generics
--import Data.Text.Lazy

data IntermediateDay = INTDAY
         { start :: Int
         , end   :: Int
         } deriving (Show, Generic, Eq, ToJSON, FromJSON, FromJSONKey)

data DayOfWeek = Mo | Tu | We | Th | Fr
           deriving (Generic, Show, Eq, Ord, Enum, FromJSON, FromJSONKey)

data Day = Day
         { name  :: DayOfWeek
         , start :: Int
         , end   :: Int
         } deriving (Show, Eq)
{-
instance FromJSON Day where
  parseJSON = withObject ""
-}
newtype Days = Days [Day] deriving (Show, Eq)

instance FromJSON Days where
  parseJSON v = ( Days
                . map  (\(nameOfDay, INTDAY st ed) -> Day nameOfDay st ed)
                . M.toList)
                <$> parseJSON v

data Section = Section
         { instructor :: String
         , sectionNum :: String
         , schedules  :: Days
         } deriving (Generic, Show, Eq, FromJSON)

data FullCourse = FullCourse
        { courseNum   :: String
        , courseTitle :: String
        , sections    :: [Section]
        } deriving (Generic, Show, Eq, FromJSON)




{-
everything: [
  fullCourse: {
    courseNum: String,
    courseTitle: String,
    sections: [
      {
        course:     fullCourse,
        instructor: String,
        type:       String,
        schedules: {
          day1: {
            start:    Int,
            end:      Int
          }
          day2: {
          start:    Int,
          end:      Int
          } ...
        }
      }
    ]
  }
]
-}
