{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (startApp) where


import           Import
import           Network.Wai.Handler.Warp
import           Server                   (app)

confirmRunning :: RIO App ()
confirmRunning = do
  env <- ask
  let p = view portL env
  let db = view databaseUrlL env
  logInfo $ fromString $ "Running on: " ++ (show p) ++ " connected to " ++ (show db)

startApp :: App -> IO ()
startApp env = do
  runRIO env confirmRunning
  run (appPort env) $ app env

