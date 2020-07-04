{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server where

import Data.Password.Argon2 (hashPassword, checkPassword)
import Database.Persist.Postgresql (toSqlKey, get, fromSqlKey, getJust, Entity (..), getBy, insertUnique)
import Import
import Models
import Network.Wai
import Servant
import Servant.Auth.Server (JWT, throwAll, makeJWT, AuthResult(..), AuthResult, JWTSettings, CookieSettings, Auth, SetCookie)
import Data.Password (PasswordCheck(..))
import RIO.ByteString.Lazy (toStrict)

{-type Api =
  "api" :> "hello" :> Get '[JSON] [Int]
    :<|> "api" :> "user" :> Get '[JSON] UserAccount
    :<|> "api" :> "users" :> ReqBody '[JSON] NewUserAccount :> Post '[JSON] UserAccount
        :<|> "api" :> "login" :> ReqBody '[JSON] Login :> Verb 'POST 204 '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                                                       , Header "Set-Cookie" SetCookie]
                                                                                       NoContent)
-}

type Protected = 
  "api" :> "user" :> Get '[JSON] UserAccount

type Unprotected = 
  "api" :> "hello" :> Get '[JSON] [Int]
    :<|> "api" :> "users" :> ReqBody '[JSON] NewUserAccount :> Post '[JSON] UserAccount
    :<|> "api" :> "login" :> ReqBody '[JSON] Login :> Post '[JSON] UserSession

type Api auths = (Auth auths AuthenticatedUser :> Protected) :<|> Unprotected

type AppM = ReaderT App Servant.Handler

protected :: AuthResult AuthenticatedUser -> ServerT Protected AppM
protected (Authenticated authUser) = (currentUser authUser)
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> ServerT Unprotected AppM
unprotected cs jwts = hello :<|> createUser cs jwts :<|> login cs jwts

-- Protected handlers

currentUser :: AuthenticatedUser -> AppM UserAccount
currentUser AuthenticatedUser{..} = do
  logInfo "Running current user"
  maybeUser <- runDB $ get $ toSqlKey auId
  logInfo $ fromString $ show maybeUser
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just user -> return user

-- Unprotected handlers

hello :: AppM [Int]
hello = do
  logInfo "Running hello"
  return [42]

createUser :: CookieSettings -> JWTSettings -> NewUserAccount -> AppM UserAccount
createUser _ _ NewUserAccount {..} = do
  -- TODO: emit a session??
  hashedPw <- hashPassword password
  maybeNewUserId <- runDB $ insertUnique $ UserAccount email hashedPw name gender birthday birthplace Nothing Nothing
  case maybeNewUserId of
    Nothing -> throwError $ err400 {errBody = "Unable to create user: duplicate email."}
    Just newUserId -> runDB $ getJust newUserId

login :: CookieSettings -> JWTSettings -> Login -> AppM UserSession
login cs jwts Login{..} = do
  maybeUser <- runDB $ getBy $ UniqueEmail loginEmail
  case maybeUser of
    Nothing -> throwError $ err401 {errBody = "Invalid email."}
    Just (Entity userId user) -> do
      case (checkPassword loginPassword (userAccountPassword user)) of
        PasswordCheckFail -> throwError $ err401 {errBody = "Invalid password."}
        PasswordCheckSuccess -> do
          token <- liftIO $ makeJWT (AuthenticatedUser (fromSqlKey userId)) jwts Nothing
          case token of
            Left e -> throwError $ err401 {errBody = "Error generating JWT."}
            Right t -> return $ UserSession $ decodeUtf8Lenient $ toStrict t



apiServer :: CookieSettings -> JWTSettings -> ServerT (Api auths) AppM
apiServer cs jwts = protected :<|> unprotected cs jwts

proxyApi :: Proxy (Api '[JWT])
proxyApi = Proxy

nt :: App -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

app :: Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> App -> Application
app cfg cs jwts ctx =
  serveWithContext proxyApi cfg $
    hoistServerWithContext proxyApi (Proxy :: Proxy [CookieSettings, JWTSettings])
      (flip runReaderT ctx) (apiServer cs jwts)