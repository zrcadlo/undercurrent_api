{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- only for the benefit of the CI instances. I'm so sorry.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import           GHC.Generics
import           RIO
import           System.Envy
import Data.Aeson.Types
import Database.Persist.Postgresql (PersistFieldSql(..), ConnectionPool)
import Database.Persist (PersistField(..), PersistValue(..), SqlType(SqlOther))
import RIO.Text (toUpper, pack, unpack, length)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Web.HttpApiData
import RIO.Time (UTCTime)

type Port = Int

type DatabaseUrl = Text

-- TODO: add an Environment (Dev, Prod, Test)
-- e.g. : https://github.com/parsonsmatt/servant-persistent/blob/9f6d4d56992a257165e775dbed514b853a2d48da/src/Config.hs

data App = App
  { appLogFunc     :: !LogFunc
  , appPort        :: !Port
  , appDatabaseUrl :: !DatabaseUrl
  , appDBPool      :: !ConnectionPool
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

class HasPort env where
  portL :: Lens' env Port
instance HasPort App where
  portL = lens appPort (\x y -> x { appPort = y})

class HasDatabaseUrl env where
  databaseUrlL :: Lens' env DatabaseUrl
instance HasDatabaseUrl App where
  databaseUrlL = lens appDatabaseUrl (\x y -> x { appDatabaseUrl = y})


class HasDBConnectionPool env where
  dbConnectionPoolL :: Lens' env ConnectionPool
instance HasDBConnectionPool App where
  dbConnectionPoolL = lens appDBPool (\x y -> x { appDBPool = y})

-- From: https://github.com/dmjio/envy/tree/68274338271bd50e265765ea524fe24918f21a3b

data EnvConfig = EnvConfig
  { port        :: Port
  , databaseUrl :: DatabaseUrl
  , jwtPath     :: FilePath
  } deriving (Generic, Show)

defaultConfig :: EnvConfig
defaultConfig = EnvConfig 3000 "postgresql://localhost/undercurrent_dev?user=luis" "JWT.key"

instance FromEnv EnvConfig

-- From: https://github.com/parsonsmatt/servant-persistent/blob/9f6d4d56992a257165e775dbed514b853a2d48da/src/Config.hs
--makeDBConnectionPool :: DatabaseUrl -> IO ConnectionPool
-- makeDBConnectionPool :: DatabaseUrl -> RIO App ConnectionPool

-- Enable ability to use `citext` types:
-- https://gist.github.com/MaxGabriel/9e757f2da60ac53b45bb06b2b097d86b
instance PersistField (CI Text) where
  toPersistValue ciText = PersistDbSpecific $ encodeUtf8 $ CI.original ciText
  fromPersistValue (PersistDbSpecific bs) = Right $ CI.mk $ decodeUtf8Lenient bs
  fromPersistValue x = Left $ pack $ "Expected PersistDBSpecific, received " <> show x
  
instance PersistFieldSql (CI Text) where
  sqlType _ = SqlOther "citext"

instance (ToJSON (CI Text)) where
  toJSON a = String $ CI.original a

instance (FromJSON (CI Text)) where
  parseJSON (String text) = pure $ CI.mk text
  parseJSON v = fail $ "Expected string, encountered " <> (show v)

-- Domain-specific types (needed here because the Models.hs module, using template haskell, wouldn't be able to
-- find them.)

data Gender = Female | Male | NonBinary
      deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance ToJSON Gender
instance FromJSON Gender

instance FromHttpApiData Gender where
  parseUrlPiece = parseBoundedTextData

instance PersistField Gender where
  toPersistValue = toPersistValueEnum
  fromPersistValue = fromPersistValueEnum

instance PersistFieldSql Gender where
  sqlType _ = SqlOther "gender"

emotionLabels :: [Text]
emotionLabels = ["joy", "trust", "anticipation", "surprise", "disgust", "sadness", "fear", "anger", "acceptance", "admiration", "affection", "annoyance", "alienation", "amazement", "anxiety", "apathy", "awe", "betrayal", "bitter", "bold", "boredom", "bravery", "brooding", "calm", "cautious", "cheerful", "comfortable", "confused", "cranky", "crushed", "curious", "denial", "despair", "disappointed", "distress", "drained", "eager", "embarassed", "empty", "energized", "envy", "excited", "foreboding", "fulfilled", "grateful", "guilt", "hatred", "shame", "helpless", "hollow", "hopeful", "humiliated", "hurt", "inspired", "intimidated", "irritated", "jealous", "lazy", "lonely", "longing", "love", "lust", "mellow", "nervous", "numb", "panic", "paranoia", "peaceful", "pity", "powerful", "powerless", "protective", "proud", "reluctance", "remorse", "resentment", "self-conscious", "sensitive", "shock", "sick", "shy", "stressed", "tired", "alert", "vigilant", "weary", "worry"]

newtype EmotionLabel = EmotionLabel Text
  deriving (Show, Eq, Generic, PersistField, PersistFieldSql, IsString)
instance ToJSON EmotionLabel
instance FromJSON EmotionLabel where
  parseJSON = withText "EmotionLabel" $ \text ->
    case (mkEmotionLabel text) of
      Just el -> pure el
      Nothing -> fail $ unpack $ text <> " isn't a known emotion."

instance FromHttpApiData EmotionLabel where
  parseUrlPiece a =
    case (mkEmotionLabel a) of
      Nothing -> Left $ a <> " is not an emotion known to our database!" 
      Just e -> Right $ e

mkEmotionLabel :: Text -> Maybe EmotionLabel
mkEmotionLabel s = 
  if s `elem` emotionLabels then
    Just $ EmotionLabel s
  else
    Nothing

data Location = Location {
  city :: Maybe Text
, region :: Maybe Text
, country :: Maybe Text
, latitude :: Maybe Double
, longitude :: Maybe Double
} deriving  (Show, Eq, Generic)

instance ToJSON Location
instance FromJSON Location

newtype Username = Username (CI Text)
  deriving (Show, Eq, Generic, PersistField, PersistFieldSql, IsString)

mkUsername :: Text -> Either Text Username
mkUsername u =
  if ((RIO.Text.length u) <= 100) then
    Right $ Username $ CI.mk u
  else
    Left "Username can't be longer than 100 characters."

instance ToJSON Username
instance FromJSON Username where
  parseJSON = withText "Username" $ \text ->
    case (mkUsername text) of
      Right u -> pure u
      Left e -> fail $ unpack e

instance FromHttpApiData Username where
  parseUrlPiece = mkUsername

newtype Email = Email (CI Text)
  deriving (Show, Eq, Generic, PersistField, PersistFieldSql, IsString)

instance ToJSON Email
instance FromJSON Email

data ZodiacSign =
    Aries
    | Taurus
    | Gemini
    | Cancer
    | Leo
    | Virgo
    | Libra
    | Scorpio
    | Sagittarius
    | Capricorn
    | Aquarius
    | Pisces
    deriving (Show, Read, Eq, Generic, Bounded, Enum)

instance PersistField ZodiacSign where
  toPersistValue = toPersistValueEnum
  fromPersistValue = fromPersistValueEnum

instance PersistFieldSql ZodiacSign where
  sqlType _ = SqlOther "zodiac_sign"

instance FromJSON ZodiacSign
instance ToJSON ZodiacSign
instance FromHttpApiData ZodiacSign where
  parseUrlPiece = parseBoundedTextData

data Range = Range {rangeStart :: UTCTime, rangeEnd :: UTCTime}

-- https://github.com/yesodweb/persistent/blob/10f689371345ec20edf0d7e8d776f7f4bc4a187b/persistent-template/Database/Persist/TH.hs#L1575
-- https://bitemyapp.com/blog/uuids-with-persistent-yesod/
-- https://github.com/yesodweb/persistent/blob/be8901eac2714e09a083c0d46bb9a2073650c497/persistent-postgresql/Database/Persist/Postgresql/JSON.hs#L325
toPersistValueEnum :: (Show e) => e -> PersistValue
toPersistValueEnum e = PersistDbSpecific . encodeUtf8 . toUpper . pack . show $ e

fromPersistValueEnum :: (Show e, Enum e, Bounded e) => PersistValue -> Either Text e
fromPersistValueEnum (PersistDbSpecific e) = parseBoundedTextData $ decodeUtf8Lenient e
fromPersistValueEnum _ = Left "Only Postgres Enums Supported"
