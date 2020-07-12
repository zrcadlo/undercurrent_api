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
import           Database.Persist.Postgresql    (Key, upsert, Entity(..), SqlPersistT, rawExecute,  runSqlPool
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
import qualified Database.Esqueleto.PostgreSQL as E
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

upsertEmotions :: (MonadIO m, HasDBConnectionPool s, MonadReader s m) => [Emotion] -> m [Key Emotion]
upsertEmotions emotions =
  forM emotions $ \emotion -> do
    Entity id_ _ <- runDB $ upsert emotion []
    return id_

allDreamEmotionIds :: (MonadIO m, HasDBConnectionPool s, MonadReader s m) => Key Dream -> m [Key Emotion]
allDreamEmotionIds dreamId = do
    entries <- 
        runDB . E.select . E.from $ \dreamEmotion -> do
            E.where_ (dreamEmotion E.^. DreamEmotionDreamId E.==. (E.val dreamId))
            return $ dreamEmotion E.^. DreamEmotionEmotionId
    return $ map E.unValue entries

userDreams :: (MonadReader s m, HasDBConnectionPool s, MonadIO m) => Key UserAccount -> Bool -> m [(Dream, [EmotionLabel])]
userDreams userId includePrivate = do
    entries <-
        runDB . E.select . E.from $ \(dream `E.InnerJoin` dreamEmotion `E.InnerJoin` emotion) -> do
            E.on  (emotion E.^. EmotionId E.==. dreamEmotion E.^. DreamEmotionEmotionId)
            E.on  (dream E.^. DreamId E.==. dreamEmotion E.^. DreamEmotionDreamId)
            E.where_ (dream E.^. DreamUserId E.==. (E.val userId) E.&&. dream E.^. DreamIsPrivate E.==. (E.val includePrivate))
            E.groupBy (dream E.^.DreamId)
            return $ (dream, E.arrayAgg $ emotion E.^. EmotionName)
    return $ map (\(Entity _ dream, el) -> (dream, maybe [] id $ E.unValue $ el)) entries

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