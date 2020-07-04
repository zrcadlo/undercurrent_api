{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings#-}
module Models where

import           Import
import           Database.Persist.TH            ( share
                                                , mkPersist
                                                , sqlSettings
                                                , mkMigrate
                                                , persistLowerCase
                                                )
import           RIO.Time                       ( UTCTime )
import           Database.Persist.Postgresql    ( runSqlPool
                                                , runMigration
                                                , SqlBackend
                                                )
import           Data.Password                  ( Password
                                                , PasswordHash(..)
                                                )
import           Data.Password.Instances        ( )
import           Data.Password.Argon2           ( Argon2
                                                , hashPassword
                                                , checkPassword
                                                )
import           Data.Aeson                     (FromJSON,  (.=)
                                                , object
                                                , ToJSON(..)
                                                )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    UserAccount
        email Text
        password (PasswordHash Argon2)
        name Text
        gender Gender
        birthday UTCTime Maybe
        birthplace Text Maybe
        createdAt UTCTime Maybe default=now()
        updatedAt UTCTime Maybe default=now()
        UniqueEmail email
        deriving Show
|]

-- manually rolling out the JSON instance for UserAccount to avoid exposing the password.
-- cf.:
-- https://www.reddit.com/r/haskell/comments/9q2plk/persistent_foreign_keys_json/e86b3vm/
-- https://github.com/yesodweb/persistent/pull/181/files (not used, but interesting)
-- https://artyom.me/aeson
instance ToJSON UserAccount where
    toJSON e = object
        [ "email" .= userAccountEmail e
        , "name" .= userAccountName e
        , "gender" .= userAccountGender e
        , "birthday" .= userAccountBirthday e
        , "birthplace" .= userAccountBirthplace e
        ]

data NewUserAccount = NewUserAccount
    { name :: Text
    , email :: Text
    , gender :: Gender
    , birthday :: Maybe UTCTime
    , birthplace :: Maybe Text
    , password :: Password
    } deriving (Show, Generic)

instance FromJSON NewUserAccount

runMigrations :: ReaderT SqlBackend IO ()
runMigrations = runMigration migrateAll

runDB
    :: (MonadReader s m, HasDBConnectionPool s, MonadIO m)
    => ReaderT SqlBackend IO b
    -> m b
runDB query = do
    pool <- view dbConnectionPoolL
    liftIO $ runSqlPool query pool
