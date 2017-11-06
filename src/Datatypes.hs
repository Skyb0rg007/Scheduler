{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Datatypes where

import           Data.Aeson
import           GHC.Generics

data DayOfWeek = Mo | Tu | We | Th | Fr | Sa | Su
           deriving (Generic, Show, Eq, Ord, Enum, ToJSON, FromJSON)

data Day = Day
         { day   :: DayOfWeek
         , start :: Int
         , end   :: Int
         } deriving (Generic, Eq, FromJSON, ToJSON)
instance Show Day where
  show (Day d s e) = show d ++ " from " ++ toTime s ++ " to " ++ toTime e
instance Ord Day where
  compare (Day d1 s1 e1) (Day d2 s2 e2)
    | d1 /= d2 = compare d1 d2
    | (e1 < s2) && (s1 < e2) = LT
    | (e2 < s1) && (s2 < e1) = GT
    | otherwise = EQ

toTime :: Int -> String
toTime i = do
  let (hr, mi) = divMod i 60
      am = hr < 12
      hr' = if am then hr else hr - 12
  show hr' ++ ":" ++ show mi ++ " " ++ (if am then "AM" else "PM")

data Section = Section
         { instructor :: String
         , sectionNum :: String
         , schedules  :: [Day]
         } deriving (Generic, Eq, FromJSON, ToJSON)
instance Show Section where
  show (Section i n ds) = n ++ " with " ++ i ++ ":\n"
                          ++ unlines (show <$> ds)

data FullCourse = FullCourse
        { courseTitle :: String
        , courseNum   :: String
        , sections    :: [Section]
        } deriving (Generic, Eq, FromJSON, ToJSON)
instance Show FullCourse where
  show (FullCourse t n ss) = t {- ++ "\n" ++ n -} ++ "\n"
                             ++ unlines (show <$> ss)

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
        schedules: [
          {
            day: String,
            start: Int,
            start: Int
          }
        ]
      }
    ]
  }
]
-}
