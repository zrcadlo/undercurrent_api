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
import Database.Persist.Postgresql (runMigration, SqlBackend)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User json
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