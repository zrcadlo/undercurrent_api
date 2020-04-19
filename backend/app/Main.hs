{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Import
import           Run
import           System.Envy (decodeEnv)

main :: IO ()
main = do
  lo <- logOptionsHandle stderr False
  env <- decodeEnv :: IO (Either String EnvConfig)
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          -- TODO: setting these to "impossible" values for now, there's gotta
          -- be a cleaner way!
          , appPort = 0
          , appDatabaseUrl = ""
          }
     in
      case env of
        Left e       -> runRIO app $ logError $ fromString $ e
        Right config -> runRIO app{ appPort = port config, appDatabaseUrl = databaseUrl config } run
