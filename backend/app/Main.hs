{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Import
import           Run
import           System.Envy (decodeWithDefaults)

main :: IO ()
main = do
  lo <- logOptionsHandle stderr False
  -- default to local db, port 3000 (see Types.hs)
  env <- decodeWithDefaults defaultConfig
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          -- TODO: setting these to "impossible" values for now, there's gotta
          -- be a cleaner way!
          , appPort = port env
          , appDatabaseUrl = databaseUrl env
          }
     in runRIO app run
