{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server where

import Data.Password.Argon2 (hashPassword, checkPassword)
import Database.Persist.Postgresql ((=.), update, toSqlKey, get, fromSqlKey, Entity (..), getBy, insertUnique)
import Import
import Models
import Network.Wai
import Servant
import Servant.Auth.Server (FromJWT, ToJWT, JWT, throwAll, makeJWT, AuthResult(..), AuthResult, JWTSettings, CookieSettings, Auth)
import Data.Password (PasswordCheck(..))
import RIO.ByteString.Lazy (toStrict)
import Data.Aeson.Types
import RIO.Time (getCurrentTime, UTCTime)
import Data.Password (Password)
import Data.Password.Instances()

-- | "Resource" types

data NewUserAccount = NewUserAccount
    { name :: Text
    , email :: Text
    , gender :: Gender
    , birthday :: Maybe UTCTime
    , birthplace :: Maybe Text
    , password :: Password
    } deriving (Show, Generic)

instance FromJSON NewUserAccount

data UpdateUserAccount = UpdateUserAccount
  {
    updateName :: Maybe Text
  , updateEmail :: Maybe Text
  , updateGender :: Maybe Gender
  , updateBirthday :: Maybe UTCTime
  , updateBirthplace :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON UpdateUserAccount where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop (length ("update_"::String)) . camelTo2 '_'}

data UpdatePassword = UpdatePassword
  {
    currentPassword :: Password
  , newPassword :: Password
  } deriving (Show, Generic)

instance FromJSON UpdatePassword

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

-- customize JSON instances: https://artyom.me/aeson#generics-handling-weird-field-names-in-data
instance FromJSON Login where
  -- drop the "login_" prefix, so we just need to say `email` and `password`
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop (length ("login_"::String)) . camelTo2 '_'}

data UserSession = UserSession
  {
    sessionToken :: Text
  , sessionUser  :: UserAccount
  } deriving (Show, Generic)

instance ToJSON UserSession where
  -- drop the `session_` prefix, so we get `token` and `user`
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop (length ("session_"::String)) . camelTo2 '_'}

-- | API types
-- inspired by: https://github.com/haskell-servant/servant-auth/tree/696fab268e21f3d757b231f0987201b539c52621#readme

type Protected = 
  "api" :> "user" :> Get '[JSON] UserAccount
    :<|> "api" :> "user" :> ReqBody '[JSON] UpdateUserAccount :> Put '[JSON] NoContent
    :<|> "api" :> "user" :> "password" :> ReqBody '[JSON] UpdatePassword :> Put '[JSON] NoContent

type Unprotected = 
  "api" :> "hello" :> Get '[JSON] [Int]
    :<|> "api" :> "users" :> ReqBody '[JSON] NewUserAccount :> Post '[JSON] UserSession
    :<|> "api" :> "login" :> ReqBody '[JSON] Login :> Post '[JSON] UserSession

type Api auths = (Auth auths AuthenticatedUser :> Protected) :<|> Unprotected

type AppM = ReaderT App Servant.Handler

-- | Handlers

protected :: AuthResult AuthenticatedUser -> ServerT Protected AppM
protected (Authenticated authUser) = (currentUser authUser) :<|> (updateUser authUser) :<|> (updatePassword authUser)
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> ServerT Unprotected AppM
unprotected cs jwts = hello :<|> createUser cs jwts :<|> login cs jwts

-- Protected handlers

currentUser :: AuthenticatedUser -> AppM UserAccount
currentUser AuthenticatedUser{..} = do
  maybeUser <- runDB $ get $ toSqlKey auId
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just user -> return user

updateUser :: AuthenticatedUser -> UpdateUserAccount -> AppM NoContent
updateUser (AuthenticatedUser auId) UpdateUserAccount {..} = do
  maybeUser <- (runDB $ get $ ((toSqlKey auId) :: Key UserAccount))
  now <- getCurrentTime
  case maybeUser of
    Nothing -> throwError $ err404 { errBody = "User not found." }
    Just _ -> do
      let updates = catMaybes $ [ maybe Nothing (Just . (UserAccountName =.))   updateName
                  , maybe Nothing (Just . (UserAccountEmail =.))  updateEmail
                  , maybe Nothing (Just . (UserAccountGender =.)) updateGender
                  , maybe Nothing (Just . (\x -> UserAccountBirthday =. Just x)) updateBirthday
                  , maybe Nothing (Just . (\x -> UserAccountBirthplace =. Just x)) updateBirthplace
                  , Just $ UserAccountUpdatedAt =. Just now]
        in
          runDB $ update (toSqlKey auId) updates
      return NoContent

updatePassword :: AuthenticatedUser -> UpdatePassword -> AppM NoContent
updatePassword (AuthenticatedUser auId) UpdatePassword {..} = do
  maybeUser <- (runDB $ get $ ((toSqlKey auId) :: Key UserAccount))
  now <- getCurrentTime
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found." }
    Just user -> do
      case (checkPassword currentPassword (userAccountPassword user)) of
        PasswordCheckFail -> throwError $ err403 {errBody = "Unable to update password" }
        PasswordCheckSuccess -> do
          pwHash <- hashPassword newPassword
          runDB $ update (toSqlKey auId) [UserAccountPassword =. pwHash, UserAccountUpdatedAt =. Just now]
          return NoContent

-- Unprotected handlers

hello :: AppM [Int]
hello = do
  logInfo "Running hello"
  return [42]

createUser :: CookieSettings -> JWTSettings -> NewUserAccount -> AppM UserSession
createUser _ jwts NewUserAccount {..} = do
  hashedPw <- hashPassword password
  now <- getCurrentTime
  maybeNewUserId <- runDB $ insertUnique $ UserAccount email hashedPw name gender birthday birthplace (Just now) (Just now)
  case maybeNewUserId of
    Nothing -> throwError $ err400 {errBody = "Unable to create user: duplicate email."}
    Just newUserId -> sessionWithUser jwts newUserId

login :: CookieSettings -> JWTSettings -> Login -> AppM UserSession
login _ jwts Login{..} = do
  maybeUser <- runDB $ getBy $ UniqueEmail loginEmail
  case maybeUser of
    Nothing -> throwError $ err401 {errBody = "Invalid email or password."}
    Just (Entity userId user) -> do
      case (checkPassword loginPassword (userAccountPassword user)) of
        PasswordCheckFail -> throwError $ err401 {errBody = "Invalid email or password."}
        PasswordCheckSuccess -> sessionWithUser jwts userId

-- | Handler helpers:

sessionWithUser :: JWTSettings -> (Key UserAccount) -> AppM UserSession
sessionWithUser jwts userId = do
  maybeUser <- runDB $ get userId
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just user -> do
      token <- liftIO $ makeJWT (AuthenticatedUser (fromSqlKey userId)) jwts Nothing
      case token of
        Left _ -> throwError $ err500 {errBody = "Unable to generate session token."}
        Right t -> return $ UserSession (decodeUtf8Lenient $ toStrict t) user

-- | Server construction

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