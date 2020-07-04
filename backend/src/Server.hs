{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Import
import           Network.Wai
import           Servant
import           Servant.API
import Models
import Database.Persist.Postgresql (getBy, Entity(..))

type Api = "api" :>
  "hello" :> Get '[JSON] [Int]
  :<|> "user" :> Get '[JSON] UserAccount

type AppM = ReaderT App Servant.Handler

hello :: AppM [Int]
hello = do
  logInfo "Running hello"
  return [42]

currentUser :: AppM UserAccount
currentUser = do
  logInfo "Running current user"
  maybeUser <- runDB $ getBy $ UniqueEmail "luis@luis.luis"
  logInfo $ fromString $ show maybeUser
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just (Entity userId user) -> return user

apiServer :: ServerT Api AppM
apiServer = hello :<|> currentUser

proxyApi :: Proxy Api
proxyApi = Proxy

nt :: App -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

app :: App -> Application
app c =  serve proxyApi $ hoistServer proxyApi (nt c) apiServer

