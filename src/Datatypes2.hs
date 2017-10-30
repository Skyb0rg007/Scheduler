{-# LANGUAGE DeriveGeneric #-}

module Dataypes2 where

import           Data.Aeson
import           GHC.Generics

data Day = Day
         { start :: Int
         , end   :: Int
         } deriving (Generic, Show, Eq)
instance ToJSON Day
instance FromJSON Day

data Section = Section
             { instructor :: String
             , classType  :: String
             , schedules  :: [Day]
             } deriving (Generic, Show, Eq)
instance ToJSON Section
instance FromJSON Section

data FullCourse = FullCourse
                { courseNum   :: String
                , courseTitle :: String
                , sections    :: [Section]
                } deriving (Generic, Show, Eq)
instance ToJSON FullCourse
instance FromJSON FullCourse





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
