{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ModelSpec (spec) where

import Import
import Test.Hspec
import Models
import Control.Monad.Logger (MonadLogger, NoLoggingT(runNoLoggingT))
import Database.Persist.Postgresql (toSqlKey, getBy, Entity(..), insert, transactionUndo, runMigration, SqlBackend, runSqlConn, withPostgresqlConn, ConnectionString)
import Data.Password.Argon2 (hashPassword)
import RIO.Time (fromGregorian, UTCTime(..))
import Util (zeroTime)


testDB :: ConnectionString
testDB = "postgresql://localhost/undercurrent_test?user=luis"

withDBConn :: (MonadIO m, MonadUnliftIO m) => ReaderT SqlBackend (NoLoggingT m) a -> m a
withDBConn = runNoLoggingT . (withPostgresqlConn testDB) . runSqlConn

prepareDB :: IO ()
prepareDB = withDBConn $ do
    _ <- runMigration migrateAll
    dropModels
    return ()

rollback :: IO ()
rollback = withDBConn $ do
    transactionUndo
    return ()

run :: ReaderT SqlBackend (NoLoggingT IO) a -> IO ()
run f =  prepareDB >> (withDBConn $ f) >> rollback


spec :: Spec
spec = do
    describe "filteredDreams" $ do
        it "works" $ do
            run $ do
                nenaDreams <- filteredDreams noDreamFilters $ Just (toSqlKey 1, False)
                liftIO $ nenaDreams `shouldBe` []
