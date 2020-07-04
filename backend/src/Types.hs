{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
module Types where

import           GHC.Generics
import           RIO
import           System.Envy
import Database.Persist.TH (derivePersistField)
import Data.Aeson.Types (ToJSON, FromJSON)
import Database.Persist.Postgresql (ConnectionPool)
import RIO.Time (UTCTime)
import Data.Password (Password)
import Data.Password.Instances()
import Servant.Auth.Server (FromJWT, ToJWT)

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


-- Domain-specific types

data Gender = Female | Male | NonBinary
      deriving (Show, Read, Eq, Generic)
derivePersistField "Gender"
instance ToJSON Gender
instance FromJSON Gender

data NewUserAccount = NewUserAccount
    { name :: Text
    , email :: Text
    , gender :: Gender
    , birthday :: Maybe UTCTime
    , birthplace :: Maybe Text
    , password :: Password
    } deriving (Show, Generic)

instance FromJSON NewUserAccount

data AuthenticatedUser = AuthenticatedUser 
  { 
    auId    :: Int64
  --, auEmail :: Text 
  } deriving (Eq, Show, Read, Generic)

instance ToJSON AuthenticatedUser
instance ToJWT  AuthenticatedUser
instance FromJSON AuthenticatedUser
instance FromJWT AuthenticatedUser

data Login = Login
  {
    loginEmail :: Text
  , loginPassword :: Password  
  } deriving (Show, Generic)

instance FromJSON Login

data UserSession = UserSession
  {
    sessionToken :: Text
  } deriving (Show, Generic)

instance ToJSON UserSession