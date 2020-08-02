{-# LANGUAGE NoImplicitPrelude #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( plus2
  , dropPrefix
  , camelToSnake
  , zeroTime
  , mkTime
  ) where

import RIO
import Data.Aeson (camelTo2)
import RIO.Time (fromGregorian, UTCTime(..))

plus2 :: Int -> Int
plus2 = (+ 2)

camelToSnake :: String -> String
camelToSnake = camelTo2 '_'

dropPrefix :: String -> String -> String
dropPrefix p = drop (length $ p <> "_") . camelToSnake

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 2020 7 7) 0

mkTime :: Integer -> Int -> Int -> UTCTime
mkTime y m d = UTCTime (fromGregorian y m d) 0
