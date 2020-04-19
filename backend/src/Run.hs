{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import           Import

run :: RIO App ()
run = do
  env <- ask
  let p = view portL env
  let db = view databaseUrlL env
  logInfo $ fromString $ "Running on: " ++ (show p) ++ " connected to " ++ (show db)
