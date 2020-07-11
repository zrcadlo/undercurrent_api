{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Types where

import           GHC.Generics
import           RIO
import           System.Envy
import Database.Persist.TH (derivePersistField)
import Data.Aeson.Types
import Database.Persist.Postgresql (PersistFieldSql, ConnectionPool)
import Database.Persist (PersistField)
import RIO.Text (unpack)

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


-- Domain-specific types (needed here because the Models.hs module, using template haskell, wouldn't be able to
-- find them.)

data Gender = Female | Male | NonBinary
      deriving (Show, Read, Eq, Generic)
derivePersistField "Gender"
instance ToJSON Gender
instance FromJSON Gender

emotionLabels :: [Text]
emotionLabels = ["joy", "trust", "anticipation", "surprise", "disgust", "sadness", "fear", "anger", "acceptance", "admiration", "affection", "annoyance", "alienation", "amazement", "anxiety", "apathy", "awe", "betrayal", "bitter", "bold", "boredom", "bravery", "brooding", "calm", "cautious", "cheerful", "comfortable", "confused", "cranky", "crushed", "curious", "denial", "despair", "disappointed", "distress", "drained", "eager", "embarassed", "empty", "energized", "envy", "excited", "foreboding", "fulfilled", "grateful", "guilt", "hatred", "shame", "helpless", "hollow", "hopeful", "humiliated", "hurt", "inspired", "intimidated", "irritated", "jealous", "lazy", "lonely", "longing", "love", "lust", "mellow", "nervous", "numb", "panic", "paranoia", "peaceful", "pity", "powerful", "powerless", "protective", "proud", "reluctance", "remorse", "resentment", "self-conscious", "sensitive", "shock", "sick", "shy", "stressed", "tired", "alert", "vigilant", "weary", "worry"]

newtype EmotionLabel = EmotionLabel Text
  deriving (Show, Eq, Generic, PersistField, PersistFieldSql)
instance ToJSON EmotionLabel
instance FromJSON EmotionLabel where
  parseJSON = withText "EmotionLabel" $ \text ->
    case (mkEmotionLabel text) of
      Just el -> pure el
      Nothing -> fail $ unpack $ text <> " isn't a known emotion."

mkEmotionLabel :: Text -> Maybe EmotionLabel
mkEmotionLabel s = 
  if s `elem` emotionLabels then
    Just $ EmotionLabel s
  else
    Nothing