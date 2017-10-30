{-# LANGUAGE TemplateHaskell #-}

module Datatypes where

import           Control.Lens
import           Data.List    (find, sort)
import           Data.Maybe


data Day = Mo | Tu | We | Th | Fr | NA
  deriving (Show, Eq, Ord)

data Schedule = Schedule { _day       :: Day
                         , _startTime :: Int
                         , _endTime   :: Int
                         } deriving (Show, Eq)
makeLenses ''Schedule

instance Ord Schedule where
  compare a b     | a == b = EQ
                  | (_day a == _day b) && (_endTime a < _startTime b) = LT
                  | (_day a == _day b) && (_endTime b < _startTime a) = GT
                  | otherwise = compare (_day a) (_day b)


data Class = Class { _name  :: String
                   , _times :: [Schedule]
                   } deriving (Show, Eq)
makeLenses ''Class

data OneClass = OC { _oCname  :: String
                   , _oCtimes :: Schedule
                   } deriving (Show, Eq)
makeLenses ''OneClass

instance Ord OneClass where
  compare a b = compare (a ^. oCtimes) (b ^. oCtimes)

sameClass :: Class -> Class -> Bool
sameClass a b = _name a == _name b

oneClass :: Day -> Class -> OneClass
oneClass d c = OC (c ^. name) (fromMaybe (Schedule NA 0 0) (find (\x -> _day x == d) (c ^. times)))

sortClass :: [Class] -> Day -> [Class]
sortClass [] _     = []
--sortClass (c:cs) d = sort (siphonClass (c:cs) d)

siphonClass :: [Class] -> Day -> [Class]
siphonClass (c:cs) d = do
  let siphoned = c & times .~ filter (\x-> _day x == d) (c ^. times)
  siphoned : sortClass cs d
siphonClass [] _ = []
