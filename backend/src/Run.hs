{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( startApp
  , makeDBConnectionPool
  , initAppConfig
  )
where
import           Control.Monad.Logger           ( runNoLoggingT )


import           Import
import           Network.Wai.Handler.Warp
import           Server                         ( app )
import           Database.Persist.Postgresql    (createPostgresqlPool
                                                , ConnectionPool
                                                )
import           System.Envy                    ( decodeWithDefaults )


confirmRunning :: RIO App ()
confirmRunning = do
  env <- ask
  let p  = view portL env
  let db = view databaseUrlL env
  logInfo
    $  fromString
    $  "Running on: "
    ++ (show p)
    ++ " connected to "
    ++ (show db)

startApp :: App -> IO ()
startApp env = do
  runRIO env confirmRunning
  run (appPort env) $ app env

makeDBConnectionPool :: DatabaseUrl -> IO ConnectionPool
makeDBConnectionPool uri =
  runNoLoggingT $ createPostgresqlPool (encodeUtf8 uri) 10

initAppConfig :: IO App
initAppConfig = do
  lo   <- logOptionsHandle stderr False
  -- default to local db, port 3000 (see Types.hs)
  env  <- decodeWithDefaults defaultConfig
  pool <- makeDBConnectionPool $ databaseUrl env
  withLogFunc lo $ \lf -> return App { appLogFunc     = lf
          -- TODO: setting these to "impossible" values for now, there's gotta
          -- be a cleaner way!
                                     , appPort        = port env
                                     , appDatabaseUrl = databaseUrl env
                                     , appDBPool      = pool
                                     }