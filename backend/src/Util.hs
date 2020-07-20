{-# LANGUAGE NoImplicitPrelude #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( plus2
  , dropPrefix
  , zeroTime
  ) where

import RIO
import Data.Aeson (camelTo2)
import RIO.Time (fromGregorian, UTCTime(..))

plus2 :: Int -> Int
plus2 = (+ 2)

dropPrefix :: String -> String -> String
dropPrefix p = drop (length $ p <> "_") . camelTo2 '_'

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 2020 7 7) 0
