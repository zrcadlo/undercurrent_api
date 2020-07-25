{-# LANGUAGE OverloadedStrings #-}

module Migrations where

import Import
import Database.PostgreSQL.Simple (connectPostgreSQL, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationCommand(..), MigrationContext(..), runMigration)


runMigrations :: FilePath -> DatabaseUrl -> IO ()
runMigrations migrationsDir conStr = do
  con <- connectPostgreSQL $ encodeUtf8 conStr
  -- initialize the `schema_migrations` table
  void $ withTransaction con $ runMigration $
    MigrationContext MigrationInitialization True con
  -- run the actual migrations
  void $ withTransaction con $ runMigration $ MigrationContext
    (MigrationDirectory migrationsDir) True con
  -- the library already prints the migrations, so we can throw out
  -- the resulting string
  return ()
