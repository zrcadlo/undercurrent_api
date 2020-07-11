{-# LANGUAGE NoImplicitPrelude #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( plus2
  , dropPrefix
  ) where

import RIO
import Data.Aeson (camelTo2)

plus2 :: Int -> Int
plus2 = (+ 2)

dropPrefix :: String -> String -> String
dropPrefix p = drop (length $ p <> "_") . camelTo2 '_'