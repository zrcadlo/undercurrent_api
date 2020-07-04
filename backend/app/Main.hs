{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Import
import           Run
import           System.Envy                    ( decodeWithDefaults )
import Options.Applicative.Simple
import qualified Paths_undercurrent_api
import Models (runMigrations)
import Database.Persist.Postgresql (runSqlPool)

data Options = Options
  {
    optionsMigrate :: !Bool
  }

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_undercurrent_api.version)
    "Undercurrent API"
    "Start the server, or run migrations."
    (Options 
      <$> switch (long "migrate"
                 <> short 'm'
                 <> help "Run migrations?"
                 )
    )
    empty

  lo <- logOptionsHandle stderr False
  -- default to local db, port 3000 (see Types.hs)
  env <- decodeWithDefaults defaultConfig
  pool <- makeDBConnectionPool $ databaseUrl env
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          -- TODO: setting these to "impossible" values for now, there's gotta
          -- be a cleaner way!
          , appPort = port env
          , appDatabaseUrl = databaseUrl env
          , appDBPool = pool
          }
     in if (optionsMigrate options) then 
        runSqlPool runMigrations pool
       else
        startApp app