{-# LANGUAGE OverloadedStrings #-}
module Helpers where


import Database.Persist.Postgresql (withPostgresqlConn, ConnectionString)
import RIO (decodeUtf8Lenient, ReaderT, MonadUnliftIO, MonadIO)
import Database.Persist.Sql (SqlBackend)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import Migrations (runMigrations)
import Database.Persist.Postgresql (runSqlConn)

testDB :: ConnectionString
testDB = "postgresql://localhost/undercurrent_test?user=luis"

withDBConn :: (MonadIO m, MonadUnliftIO m) => ReaderT SqlBackend (NoLoggingT m) a -> m a
withDBConn = runNoLoggingT . (withPostgresqlConn testDB) . runSqlConn

migrate :: IO ()
migrate = do
    didMigrate <- runMigrations "migrations" $ decodeUtf8Lenient testDB
    case didMigrate of
        Right s -> putStrLn $ "Migrated!" <> s
        Left e -> putStrLn $ "Error migrating: " <> e
