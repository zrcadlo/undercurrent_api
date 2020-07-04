{-# LANGUAGE RecordWildCards #-}
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
import Database.Persist.Postgresql (insert, get, getBy, Entity(..))
import Data.Password.Argon2  (hashPassword, checkPassword)

type Api = 
       "api" :> "hello" :> Get '[JSON] [Int] 
  :<|> "api" :> "user"  :> Get '[JSON] UserAccount
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
    Just (Entity userId user) -> return user

createUser :: NewUserAccount -> AppM UserAccount
createUser NewUserAccount{..} = do
  hashedPw <- hashPassword password
  newUserId <- runDB $ insert $ UserAccount email hashedPw name gender birthday birthplace Nothing Nothing
  maybeNewUser <- runDB $ get newUserId
  case maybeNewUser of
    Nothing -> throwError $ err400 {errBody = "Unable to create user"}
    Just newUser -> return newUser

apiServer :: ServerT Api AppM
apiServer = hello :<|> currentUser :<|> createUser

proxyApi :: Proxy Api
proxyApi = Proxy

nt :: App -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

app :: App -> Application
app c =  serve proxyApi $ hoistServer proxyApi (nt c) apiServer

