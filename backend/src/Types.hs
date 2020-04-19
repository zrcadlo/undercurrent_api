{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import           GHC.Generics
import           RIO
import           System.Envy

type Port = Int

type DatabaseUrl = String

data App = App
  { appLogFunc     :: !LogFunc
  , appPort        :: !Port
  , appDatabaseUrl :: !DatabaseUrl
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

-- From: https://github.com/dmjio/envy/tree/68274338271bd50e265765ea524fe24918f21a3b

data EnvConfig = EnvConfig
  { port        :: Port
  , databaseUrl :: DatabaseUrl
  } deriving (Generic, Show)

defaultConfig :: EnvConfig
defaultConfig = EnvConfig 3000 "postgresql://localhost/undercurrent_dev?user=luis"

instance FromEnv EnvConfig
