{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server where

import Data.Password.Argon2 (hashPassword)
import Database.Persist.Postgresql (getJust, Entity (..), getBy, insertUnique)
import Import
import Models
import Network.Wai
import Servant

type Api =
  "api" :> "hello" :> Get '[JSON] [Int]
    :<|> "api" :> "user" :> Get '[JSON] UserAccount
    :<|> "api" :> "users" :> ReqBody '[JSON] NewUserAccount :> Post '[JSON] UserAccount

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
    Just (Entity _ user) -> return user

createUser :: NewUserAccount -> AppM UserAccount
createUser NewUserAccount {..} = do
  hashedPw <- hashPassword password
  maybeNewUserId <- runDB $ insertUnique $ UserAccount email hashedPw name gender birthday birthplace Nothing Nothing
  case maybeNewUserId of
    Nothing -> throwError $ err400 {errBody = "Unable to create user: duplicate email."}
    Just newUserId -> runDB $ getJust newUserId

apiServer :: ServerT Api AppM
apiServer = hello :<|> currentUser :<|> createUser

proxyApi :: Proxy Api
proxyApi = Proxy

nt :: App -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

app :: App -> Application
app c = serve proxyApi $ hoistServer proxyApi (nt c) apiServer
