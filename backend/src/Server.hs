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
import Network.HTTP.Types (status200)
import Servant
import Servant.Auth.Server (FromJWT, ToJWT, JWT, throwAll, makeJWT, AuthResult(..), AuthResult, JWTSettings, CookieSettings, Auth)
import Data.Password (PasswordCheck(..))
import RIO.ByteString.Lazy (fromStrict, toStrict)
import RIO.Text as T (pack)
import Data.Aeson.Types
import RIO.Time (fromGregorian, getCurrentTime, UTCTime(..))
import Data.Password (Password)
import Data.Password.Instances()
import Servant.Docs
import Servant.Auth.Docs()

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
instance ToJSON NewUserAccount where
  toJSON NewUserAccount{..} = object
    [
      "name" .= name
    , "email" .= email
    , "gender" .= gender
    , "birthday" .= birthday
    , "birthplace" .= birthplace
    , "password" .= ("somePassword"::Text)
    ]

data UpdateUserAccount = UpdateUserAccount
  {
    updateName :: Maybe Text
  , updateEmail :: Maybe Text
  , updateGender :: Maybe Gender
  , updateBirthday :: Maybe UTCTime
  , updateBirthplace :: Maybe Text
  } deriving (Show, Generic)

dropUpdatePrefix :: String -> String
dropUpdatePrefix = drop (length ("update_"::String)) . camelTo2 '_'

instance FromJSON UpdateUserAccount where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = dropUpdatePrefix}

instance ToJSON UpdateUserAccount where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = dropUpdatePrefix}

data UpdatePassword = UpdatePassword
  {
    currentPassword :: Password
  , newPassword :: Password
  } deriving (Show, Generic)

instance FromJSON UpdatePassword
instance ToJSON UpdatePassword where
  toJSON _ = object
    [ "currentPassword" .= ("sample"::Text)
    , "newPassword" .=  ("anotherPassword"::Text)
    ]

newtype UserId = UserId {userId :: Int64}
  deriving (Eq, Show, Read, Generic)

instance FromJSON UserId
instance ToJSON UserId

data AuthenticatedUser = AuthenticatedUser 
  { 
    auId    :: UserId
  --, auEmail :: Text 
  } deriving (Eq, Show, Read, Generic)

instance ToJSON AuthenticatedUser
instance ToJWT  AuthenticatedUser
instance FromJSON AuthenticatedUser
instance FromJWT AuthenticatedUser

data NewDream = NewDream
  {
    title :: Text
  , date  :: UTCTime
  , description :: Text
  , emotions :: [EmotionLabel]
  , lucid :: Bool
  , nightmare :: Bool
  , recurring :: Bool
  , private :: Bool
  , starred :: Bool
  } deriving (Eq, Show, Generic)

data Login = Login
  {
    loginEmail :: Text
  , loginPassword :: Password  
  } deriving (Show, Generic)

-- customize JSON instances: https://artyom.me/aeson#generics-handling-weird-field-names-in-data
instance FromJSON Login where
  -- drop the "login_" prefix, so we just need to say `email` and `password`
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop (length ("login_"::String)) . camelTo2 '_'}

instance ToJSON Login where
  toJSON Login{..} = object
    [
      "email" .= loginEmail
    , "password" .= ("somePassword"::Text)
    ]

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
    :<|> "api" :> "user" :> ReqBody '[JSON] UpdateUserAccount :> Verb 'PUT 204 '[JSON] NoContent
    :<|> "api" :> "user" :> "password" :> ReqBody '[JSON] UpdatePassword :> Verb 'PUT 204 '[JSON] NoContent

type Unprotected = 
    "api" :> "users" :> ReqBody '[JSON] NewUserAccount :> PostCreated '[JSON] UserSession
    :<|> "api" :> "login" :> ReqBody '[JSON] Login :> PostCreated '[JSON] UserSession

type Static =
  "docs" :> Raw

type Api auths = (Auth auths AuthenticatedUser :> Protected) :<|> Unprotected :<|> Static

type AppM = ReaderT App Servant.Handler

-- | Documentation instances

-- NOTE: DB models have their instances defined in Models.hs
instance ToSample NewUserAccount where
  toSamples _ = singleSample $
    NewUserAccount  "Paco Alpaco"
      "paco@alpaca.net"
      Male
      (Just (UTCTime (fromGregorian 2017 2 14) 0))
      (Just "Shenzhen, China")
      "secureAlpacaPassword"

instance ToSample UpdateUserAccount where
  toSamples _ = singleSample $
    UpdateUserAccount (Just "New Alpaca Name")
      (Just "new.email@alpaca.net")
      (Just NonBinary)
      Nothing
      Nothing

instance ToSample UpdatePassword where
  toSamples _ = singleSample $
    UpdatePassword "newPassword" "newPassword"

instance ToSample Login where
  toSamples _ = singleSample $
    Login "charlie@alpaca.net" "password"

instance ToSample UserSession where
  toSamples _ = singleSample $
    UserSession "some-long-token" sampleUser

instance ToSample AuthenticatedUser where
  toSamples _ = singleSample $ AuthenticatedUser $ UserId 42

-- | Handlers

protected :: AuthResult AuthenticatedUser -> ServerT Protected AppM
protected (Authenticated authUser) = (currentUser authUser) :<|> (updateUser authUser) :<|> (updatePassword authUser)
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> ServerT Unprotected AppM
unprotected cs jwts = createUser cs jwts :<|> login cs jwts

-- Protected handlers

currentUser :: AuthenticatedUser -> AppM UserAccount
currentUser AuthenticatedUser{..} = do
  maybeUser <- runDB $ get $ toSqlKey $ userId auId
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just user -> return user

updateUser :: AuthenticatedUser -> UpdateUserAccount -> AppM NoContent
updateUser (AuthenticatedUser auId) UpdateUserAccount {..} = do
  maybeUser <- (runDB $ get $ ((toSqlKey (userId auId)) :: Key UserAccount))
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
          runDB $ update (toSqlKey $ userId auId) updates
      return NoContent

updatePassword :: AuthenticatedUser -> UpdatePassword -> AppM NoContent
updatePassword (AuthenticatedUser auId) UpdatePassword {..} = do
  maybeUser <- (runDB $ get $ ((toSqlKey (userId auId)) :: Key UserAccount))
  now <- getCurrentTime
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found." }
    Just user -> do
      case (checkPassword currentPassword (userAccountPassword user)) of
        PasswordCheckFail -> throwError $ err403 {errBody = "Unable to update password" }
        PasswordCheckSuccess -> do
          pwHash <- hashPassword newPassword
          runDB $ update (toSqlKey (userId auId)) [UserAccountPassword =. pwHash, UserAccountUpdatedAt =. Just now]
          return NoContent

-- Unprotected handlers

createUser :: CookieSettings -> JWTSettings -> NewUserAccount -> AppM UserSession
createUser _ jwts NewUserAccount {..} = do
  hashedPw <- hashPassword password
  now <- getCurrentTime
  maybeNewUserId <- runDB $ insertUnique $ UserAccount email hashedPw name gender birthday birthplace (Just now) (Just now)
  case maybeNewUserId of
    Nothing -> throwError $ err409 {errBody = "Unable to create user: duplicate email."}
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
sessionWithUser jwts sessionUserId = do
  maybeUser <- runDB $ get sessionUserId
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just user -> do
      token <- liftIO $ makeJWT (AuthenticatedUser (UserId $ fromSqlKey $ sessionUserId)) jwts Nothing
      case token of
        Left _ -> throwError $ err500 {errBody = "Unable to generate session token."}
        Right t -> return $ UserSession (decodeUtf8Lenient $ toStrict t) user

-- | Server construction

docsH :: Tagged AppM (p -> (Network.Wai.Response -> t) -> t)
docsH = return serveDocs where
  serveDocs _ respond =
    respond $ responseLBS status200 [plain] (fromStrict docsBs)
  plain = ("Content-Type", "text/plain")
  docsBs = encodeUtf8
         . T.pack
         . markdown
         $ docsWithIntros [intro] proxyApi
  intro = DocIntro "Undercurrent API" ["For an up-to-date version of this documentation, visit the `/docs` endpoint of the API.\
                                       \For all `JWT` protected endpoints, you must provide it in the `Authorization` header, with a value of `Bearer THE_TOKEN`\
                                       \(where `THE_TOKEN` is what's returned in the `token` property after logging in or creating a user.)"]

apiServer :: CookieSettings -> JWTSettings -> ServerT (Api auths) AppM
apiServer cs jwts = protected :<|> unprotected cs jwts :<|> docsH

proxyApi :: Proxy (Api '[JWT])
proxyApi = Proxy

nt :: App -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

app :: Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> App -> Application
app cfg cs jwts ctx =
  serveWithContext proxyApi cfg $
    hoistServerWithContext proxyApi (Proxy :: Proxy [CookieSettings, JWTSettings])
      (flip runReaderT ctx) (apiServer cs jwts)