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
import           Database.Persist.Postgresql    (Key, Entity(..), SqlPersistT, rawExecute,  runSqlPool
                                                , runMigration
                                                , SqlBackend
                                                )
import           Data.Password                  ( PasswordHash(..)
                                                )
import           Data.Password.Instances        ( )
import           Data.Password.Argon2           ( Argon2
                                                
                                                
                                                )
import Servant.Docs
import Data.Aeson.Types
import qualified Database.Esqueleto as E
-- uncomment for special operators like arrayAgg
--import qualified Database.Esqueleto.PostgreSQL as E
import Database.Esqueleto.PostgreSQL.JSON (JSONB)


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

    Dream
        userId UserAccountId
        title Text
        description Text
        isLucid Bool
        isNightmare Bool
        isRecurring Bool
        isPrivate Bool
        isStarred Bool
        emotions  (JSONB [EmotionLabel])
        dreamedAt UTCTime default=now()
        createdAt UTCTime default=now()
        updatedAt UTCTime default=now()

    Emotion
        name EmotionLabel
        createdAt UTCTime default=now()
        updatedAt UTCTime default=now()
        UniqueEmotionName name

    DreamEmotion
        dreamId DreamId
        emotionId EmotionId
        createdAt UTCTime default=now()
        updatedAt UTCTime default=now()
        UniqueDreamEmotion dreamId emotionId

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

runMigrations :: ReaderT SqlBackend IO ()
runMigrations = runMigration migrateAll

runDB
    :: (MonadReader s m, HasDBConnectionPool s, MonadIO m)
    => ReaderT SqlBackend IO b
    -> m b
runDB query = do
    pool <- view dbConnectionPoolL
    liftIO $ runSqlPool query pool

-- | Queries

userDreams :: (MonadReader s m, HasDBConnectionPool s, MonadIO m) => Key UserAccount -> Bool -> m [Entity Dream]
userDreams userId includePrivate = do
    entries <-
        runDB . E.select . E.from $ \dream -> do
            E.where_ (dream E.^. DreamUserId E.==. (E.val userId) E.&&. dream E.^. DreamIsPrivate E.==. (E.val includePrivate))
            return dream
    return entries

-- | Documentation helpers

sampleUser :: UserAccount
sampleUser = 
    UserAccount "nena@alpaca.com" 
      (PasswordHash "secureAlpacaPassword")
      "Nena Alpaca"
      Female
      Nothing
      (Just "Tokyo, Japan")
      Nothing
      Nothing

instance ToSample UserAccount where
  toSamples _ = singleSample sampleUser

-- | Concessions to testing

dropModels :: (MonadIO m) => SqlPersistT m ()
dropModels = rawExecute "TRUNCATE TABLE user_account, dream, emotion, dream_emotion RESTART IDENTITY" []