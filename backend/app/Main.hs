{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Import
import           Run
import           System.Envy                    ( decodeWithDefaults )
import Options.Applicative.Simple
import qualified Paths_undercurrent_api
import Migrations (runMigrations)
import Servant.Server (Context(..))
import Servant.Auth.Server (readKey, defaultCookieSettings, defaultJWTSettings)

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
  jwtKey <- readKey $ jwtPath env
  pool <- makeDBConnectionPool $ databaseUrl env
  let logOptions = setLogUseTime True lo
  withLogFunc logOptions $ \lf ->
    let app = App
          { appLogFunc = lf
          , appPort = port env
          , appDatabaseUrl = databaseUrl env
          , appDBPool = pool
          }
        jwtCfg = defaultJWTSettings jwtKey
        cookieCfg = defaultCookieSettings
        cfg = cookieCfg :. jwtCfg :. EmptyContext
     in if (optionsMigrate options) then
       runRIO app $ do
          didMigrate <- liftIO $ runMigrations "migrations" (databaseUrl env)
          case didMigrate of
            Left _ -> logInfo "Error migrating!"
            Right _ -> logInfo "All migrations ran!"
       else
        startApp cfg cookieCfg jwtCfg app
