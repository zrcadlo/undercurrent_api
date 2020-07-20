{-# LANGUAGE OverloadedStrings #-}

module Migrations where

import Database.Persist.Sql (Sql)

{- NOTE(luis)
All migrations here should be idempotent: assume they'll be run against an up-to-date schema at some point:
- Use `CREATE OR REPLACE` for stored procedures.
- Use `Drop trigger if exists ... create trigger` for triggers.
- Use `CREATE INDEX [CONCURRENTLY] IF NOT EXISTS` for indices.
-}

enableCitext :: Sql
enableCitext = "CREATE EXTENSION IF NOT EXISTS citext"

addTimestampFunctions :: Sql
addTimestampFunctions = "CREATE OR REPLACE FUNCTION create_timestamps()   \
\        RETURNS TRIGGER AS $$\
\        BEGIN\
\            NEW.created_at = now();\
\            NEW.updated_at = now();\
\            RETURN NEW;   \
\        END;\
\        $$ language 'plpgsql';\
\ \
\CREATE OR REPLACE FUNCTION update_timestamps()   \
\        RETURNS TRIGGER AS $$\
\        BEGIN\
\            NEW.updated_at = now();\
\            RETURN NEW;   \
\        END;\
\        $$ language 'plpgsql';"

addUserTriggers :: Sql
addUserTriggers = 
    "DROP TRIGGER IF EXISTS user_account_insert ON user_account;\
      \CREATE TRIGGER user_account_insert BEFORE INSERT ON user_account FOR EACH ROW EXECUTE PROCEDURE create_timestamps();\
    \DROP TRIGGER IF EXISTS user_account_update ON user_account;\
      \CREATE TRIGGER user_account_update BEFORE UPDATE ON user_account FOR EACH ROW EXECUTE PROCEDURE update_timestamps();"

addDreamTriggers :: Sql
addDreamTriggers =
    "DROP TRIGGER IF EXISTS dream_insert ON dream;\
      \CREATE TRIGGER dream_insert BEFORE INSERT ON dream FOR EACH ROW EXECUTE PROCEDURE create_timestamps();\
    \DROP TRIGGER IF EXISTS dream_update ON dream;\
      \CREATE TRIGGER dream_update BEFORE UPDATE ON dream FOR EACH ROW EXECUTE PROCEDURE update_timestamps();"
