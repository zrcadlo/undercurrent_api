{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes                #-}
module Models where

import           Import
import           Database.Persist.TH            ( share
                                                , mkPersist
                                                , sqlSettings
                                                , mkMigrate
                                                , persistLowerCase
                                                )
import RIO.Time (UTCTime)
import Database.Persist.Postgresql (runSqlPool, runMigration, SqlBackend)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    UserAccount json
        name Text
        email Text
        password Text
        gender Gender
        birthday UTCTime
        birthplace Text
        createdAt UTCTime default=now()
        UniqueEmail email
        deriving Show
|]

runMigrations :: ReaderT SqlBackend IO ()
runMigrations = runMigration migrateAll

runDB :: (MonadReader s m, HasDBConnectionPool s, MonadIO m) => ReaderT SqlBackend IO b -> m b
runDB query = do
    pool <- view dbConnectionPoolL
    liftIO $ runSqlPool query pool 