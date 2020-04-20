{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Import
import           Network.Wai
import           Servant
import           Servant.API

type Api = "api" :>
  "hello" :> Get '[JSON] [Int]

type AppM = ReaderT App Servant.Handler

hello :: AppM [Int]
hello = do
  logInfo "Running hello"
  return [42]

apiServer :: ServerT Api AppM
apiServer = hello

proxyApi :: Proxy Api
proxyApi = Proxy

nt :: App -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

app :: App -> Application
app c =  serve proxyApi $ hoistServer proxyApi (nt c) apiServer

