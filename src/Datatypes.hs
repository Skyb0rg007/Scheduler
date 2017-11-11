{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

--Where the underlying datatypes are located
--Includes their compare and show definitions
module Datatypes (FullCourse (..), Section (..), Day (..), DayOfWeek) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

--Pretty self-explanatory for what this represents
data DayOfWeek = Mo | Tu | We | Th | Fr | Sa | Su
           deriving (Generic, Show, Eq, Ord, Enum, ToJSON, FromJSON)

--One schedule: day, start, and end
--Ex. Tuesdays from 12 noon to 12:50
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

--Tufts uses time as number of minutes past midnight, so this converts back
toTime :: Int -> String
toTime i = do
  let (hr, mi) = divMod i 60
      am = hr < 12
      hr' = if am || hr == 12 then hr else hr - 12
  show hr' ++ ":" ++ showMin mi ++ " " ++ (if am then "AM" else "PM")

--ensures 2 places are shown
showMin :: Int -> String
showMin i = case length $ show i of
  1 -> '0':show i
  2 -> show i
  _ -> error "Why are you trying to make a time with over 99 minutes?"

--One "class": the final answer is a bunch of these
--Ex. Calc 34-03 with Mary Glacier
data Section = Section
         { instructor :: String
         , sectionNum :: String
         , schedules  :: [Day]
         } deriving (Generic, Eq, FromJSON, ToJSON)
instance Show Section where
  show (Section i n ds) = n ++ " with " ++ i ++ ":\n"
                          ++ unlines (show <$> ds)

--All of the possibilities. The initial input is a bunch of these
--Ex. Calc 34
data FullCourse = FullCourse
        { courseTitle :: String
        , courseNum   :: String
        , sections    :: [Section]
        } deriving (Generic, Eq, FromJSON, ToJSON)
instance Show FullCourse where
  show (FullCourse t n ss) = "\n" ++ t ++ "\n"
                             ++ unlines (show <$> ss) ++ "\n"



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
