{-# LANGUAGE OverloadedStrings #-}

module Migrations where

import Import
import Database.PostgreSQL.Simple (connectPostgreSQL, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationResult(..), MigrationCommand(..), MigrationContext(..), runMigration)

-- TODO(luis) we may want to take a Bool parameter to send
-- in `MigrationContext`: right now it defaults to verbose.
runMigrations :: FilePath -> DatabaseUrl -> IO (Either String String)
runMigrations migrationsDir conStr = do
  con <- connectPostgreSQL $ encodeUtf8 conStr
  -- initialize the `schema_migrations` table
  void $ withTransaction con $ runMigration $
    MigrationContext MigrationInitialization True con
  -- run the actual migrations
  result <- withTransaction con $ runMigration $ MigrationContext
              (MigrationDirectory migrationsDir) True con

  case result of
    MigrationSuccess -> pure $ Right "All migrations ran."
    MigrationError e -> pure $ Left e
