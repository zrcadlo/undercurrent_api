{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server where

import Data.Password.Argon2 (hashPassword, checkPassword)
import Database.Persist.Postgresql (delete, insert, (=.), update, toSqlKey, get, fromSqlKey, Entity (..), getBy, insertUnique)
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
import Util
import RIO.Partial (fromJust)
import Database.Esqueleto.PostgreSQL.JSON (JSONB(JSONB))

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

instance ToSample NewUserAccount where
  toSamples _ = singleSample $
    NewUserAccount  "Paco Alpaco"
      "paco@alpaca.net"
      Male
      (Just (UTCTime (fromGregorian 2017 2 14) 0))
      (Just "Shenzhen, China")
      "secureAlpacaPassword"

data UpdateUserAccount = UpdateUserAccount
  {
    updateName :: Maybe Text
  , updateEmail :: Maybe Text
  , updateGender :: Maybe Gender
  , updateBirthday :: Maybe UTCTime
  , updateBirthplace :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON UpdateUserAccount where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = dropPrefix "update"}

instance ToJSON UpdateUserAccount where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = dropPrefix "update"}

instance ToSample UpdateUserAccount where
  toSamples _ = singleSample $
    UpdateUserAccount (Just "New Alpaca Name")
      (Just "new.email@alpaca.net")
      (Just NonBinary)
      Nothing
      Nothing

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

instance ToSample UpdatePassword where
  toSamples _ = singleSample $
    UpdatePassword "newPassword" "newPassword"

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

instance ToSample AuthenticatedUser where
  toSamples _ = singleSample $ AuthenticatedUser $ UserId 42

data DreamWithEmotions = DreamWithEmotions
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

instance FromJSON DreamWithEmotions
instance ToJSON DreamWithEmotions

instance ToSample DreamWithEmotions where
  toSamples _ = 
    singleSample $ 
      DreamWithEmotions "I dream of Alpacas"
        zeroTime
        "Some alpacas were wearing sunglasses"
        (map (fromJust . mkEmotionLabel) ["joy", "intimidated"])
        False
        False
        True
        False
        True

data DreamMeta = DreamMeta
  {
    id :: Key Dream
  , createdAt :: UTCTime
  } deriving (Eq, Show, Generic)

instance FromJSON DreamMeta
instance ToJSON DreamMeta

instance ToSample DreamMeta where
  toSamples _ =  [("Useful information for a just-created dream", DreamMeta {id = toSqlKey 42, createdAt = zeroTime})]

data DreamUpdate = DreamUpdate
  { 
    updateTitle :: Maybe Text
  , updateDate  :: Maybe UTCTime
  , updateDescription :: Maybe Text
  , updateEmotions :: Maybe [EmotionLabel]
  , updateLucid :: Maybe Bool
  , updateNightmare :: Maybe Bool
  , updateRecurring :: Maybe Bool
  , updatePrivate :: Maybe Bool
  , updateStarred :: Maybe Bool
  } deriving (Eq, Show, Generic)

instance FromJSON DreamUpdate where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = dropPrefix "update"}

instance ToJSON DreamUpdate where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = dropPrefix "update"}

instance ToSample DreamUpdate where
  toSamples _ = 
    [("All fields are optional; if emotions are sent, they will replace current ones.", sampleDreamUpdate)]
    where
      sampleDreamUpdate = 
        DreamUpdate (Just "I dreamed a dream")
          Nothing
          Nothing
          (Just (map (fromJust . mkEmotionLabel) ["acceptance"]))
          (Just True)
          (Just False)
          (Just False)
          (Just True)
          (Just False)


data Login = Login
  {
    loginEmail :: Text
  , loginPassword :: Password  
  } deriving (Show, Generic)

-- customize JSON instances: https://artyom.me/aeson#generics-handling-weird-field-names-in-data
instance FromJSON Login where
  -- drop the "login_" prefix, so we just need to say `email` and `password`
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = dropPrefix "login"}

instance ToJSON Login where
  toJSON Login{..} = object
    [
      "email" .= loginEmail
    , "password" .= ("somePassword"::Text)
    ]

instance ToSample Login where
  toSamples _ = singleSample $
    Login "charlie@alpaca.net" "password"

data UserSession = UserSession
  {
    sessionToken :: Text
  , sessionUser  :: UserAccount
  } deriving (Show, Generic)

instance ToJSON UserSession where
  -- drop the `session_` prefix, so we get `token` and `user`
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = dropPrefix "session" }


instance ToSample UserSession where
  toSamples _ = singleSample $
    UserSession "some-long-token" sampleUser

-- sad trombone: orphan instances. The servant docs totally expect this:
-- {-# OPTIONS_GHC -fno-warn-orphans #-}
instance ToCapture (Capture "dreamId" Int64) where
  toCapture _ = DocCapture "dreamId" "ID of the dream to update, as returned when creating it."

-- TODO: need an "external id"??
instance ToCapture (Capture "userId" Int64) where
  toCapture _ = DocCapture "userId" "ID of the user to inspect, as returned when creating it."

-- | API types
-- inspired by: https://github.com/haskell-servant/servant-auth/tree/696fab268e21f3d757b231f0987201b539c52621#readme

type Protected = 
  "api" :> "user" :> Get '[JSON] UserAccount
    :<|> "api" :> "user" :> ReqBody '[JSON] UpdateUserAccount :> Verb 'PUT 204 '[JSON] NoContent
    :<|> "api" :> "user" :> "password" :> ReqBody '[JSON] UpdatePassword :> Verb 'PUT 204 '[JSON] NoContent
    :<|> "api" :> "user" :> "dreams" :> ReqBody '[JSON] DreamWithEmotions :> PostCreated '[JSON] DreamMeta
    :<|> "api" :> "user" :> "dreams" :> Capture "dreamId" Int64 :> ReqBody '[JSON] DreamUpdate :> Verb 'PUT 204 '[JSON] NoContent
    :<|> "api" :> "user" :> "dreams" :> Capture "dreamId" Int64 :> Verb 'DELETE 204 '[JSON] NoContent
    :<|> "api" :> "user" :> "dreams" :> Get '[JSON] [DreamWithEmotions]

type Unprotected = 
    "api" :> "users" :> ReqBody '[JSON] NewUserAccount :> PostCreated '[JSON] UserSession
    :<|> "api" :> "login" :> ReqBody '[JSON] Login :> PostCreated '[JSON] UserSession

-- TODO(luis) add a limit/pagination
type KindaProtected =
  "api" :> "users" :> Capture "userId" Int64 :> "dreams" :> Get '[JSON] [DreamWithEmotions] 

type Static =
  "docs" :> Raw

type Api auths = (Auth auths AuthenticatedUser :> Protected) 
  :<|> Unprotected 
  :<|> (Auth auths AuthenticatedUser :> KindaProtected)
  :<|> Static

type AppM = ReaderT App Servant.Handler

-- | Handlers

protected :: AuthResult AuthenticatedUser -> ServerT Protected AppM
protected (Authenticated authUser) = (currentUser authUser) 
  :<|> (updateUser authUser)
  :<|> (updatePassword authUser)
  :<|> (createDream authUser)
  :<|> (updateDream authUser)
  :<|> (deleteDream authUser)
  :<|> (myDreams authUser)
protected _ = throwAll err401

kindaProtected :: AuthResult AuthenticatedUser -> ServerT KindaProtected AppM
kindaProtected authResult = allUserDreams authResult

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

createDream :: AuthenticatedUser -> DreamWithEmotions -> AppM DreamMeta
createDream (AuthenticatedUser auId) DreamWithEmotions{..} = do
  let dbDream = Dream (toSqlKey $ userId $ auId)
        title
        description
        lucid
        nightmare
        recurring
        private
        starred
        (JSONB emotions)
        date
        zeroTime
        zeroTime
    
  now <- getCurrentTime
  dreamId <- runDB $ insert dbDream
  return $ DreamMeta dreamId now

updateDream :: AuthenticatedUser -> Int64 -> DreamUpdate -> AppM NoContent
updateDream (AuthenticatedUser auId) dreamId updates = do
  let dreamKey = toSqlKey dreamId :: Key Dream
  let userKey  = toSqlKey $ userId auId :: Key UserAccount 
  maybeDream <- runDB $ get dreamKey
  case maybeDream of
    Nothing -> throwError $ err404 { errBody = "Dream not found."}
    Just existingDream -> do
      if (userKey == (dreamUserId existingDream)) then
        updateDreamH dreamKey updates >> return NoContent
      else
        throwError $ err403 {errBody = "This is not your dream."}

deleteDream :: AuthenticatedUser -> Int64 -> AppM NoContent
deleteDream (AuthenticatedUser auId) dreamId = do
  let dreamKey = toSqlKey dreamId :: Key Dream
  let userKey  = toSqlKey $ userId auId :: Key UserAccount
  maybeDream <- runDB $ get dreamKey
  case maybeDream of
    Nothing -> throwError $ err410 { errBody = "The dream is gone." }
    Just existingDream -> do
      if (userKey == (dreamUserId existingDream)) then
        runDB (delete dreamKey) >> return NoContent
      else
        throwError $ err403 {errBody = "This is not your dream."}

-- TODO: do we need pagination and filtering here?
myDreams :: AuthenticatedUser -> AppM [DreamWithEmotions]
myDreams (AuthenticatedUser auId) = do
  let userKey = toSqlKey $ userId auId :: Key UserAccount
  allMyDreams <- userDreams userKey True
  -- TODO: this is doing a double `map` (here and in Models.hs)
  -- maybe we can compose those in one place vs. in two?
  return $ map dreamWithEmotions allMyDreams

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

-- Handlers that check their own authentication

-- TODO: do we need to apply filters here, too?
allUserDreams :: AuthResult AuthenticatedUser -> Int64 -> AppM [DreamWithEmotions]
allUserDreams authResult requestedId = do
  let requestedUserId = toSqlKey requestedId :: Key UserAccount
  let isOwner = case authResult of
                  Authenticated (AuthenticatedUser auId) -> (toSqlKey $ userId $ auId) == requestedUserId
                  _ -> False
  requestedDreams <- userDreams requestedUserId isOwner
  return $ map dreamWithEmotions requestedDreams

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

dreamWithEmotions :: (Dream, [EmotionLabel]) -> DreamWithEmotions
dreamWithEmotions (Dream{..}, els) =
  DreamWithEmotions
    {
      title = dreamTitle
    , date  = dreamDreamedAt
    , description = dreamDescription
    , emotions = els
    , lucid = dreamIsLucid
    , nightmare = dreamIsNightmare
    , recurring = dreamIsRecurring
    , private = dreamIsPrivate
    , starred = dreamIsStarred
    } 

toEmotion :: EmotionLabel -> Emotion
toEmotion e = Emotion e zeroTime zeroTime

updateDreamH :: (MonadReader s m, HasDBConnectionPool s, MonadIO m) => Key Dream -> DreamUpdate -> m ()
updateDreamH dreamKey DreamUpdate{..} =
    let updates = catMaybes $ [ maybe Nothing (Just . (DreamTitle =.)) updateTitle 
                , maybe Nothing (Just . (DreamDreamedAt =.)) updateDate
                , maybe Nothing (Just . (DreamDescription =.)) updateDescription
                , maybe Nothing (Just . (DreamIsLucid =.)) updateLucid
                , maybe Nothing (Just . (DreamIsNightmare =.)) updateNightmare
                , maybe Nothing (Just . (DreamIsRecurring =.)) updateRecurring
                , maybe Nothing (Just . (DreamIsPrivate =.)) updatePrivate
                , maybe Nothing (Just . (DreamIsStarred =.)) updateStarred
                , maybe Nothing (Just . (\e -> DreamEmotions =. JSONB e)) updateEmotions
                ]
    in
        runDB $ update dreamKey updates

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
apiServer cs jwts = protected 
  :<|> unprotected cs jwts
  :<|> kindaProtected
  :<|> docsH

proxyApi :: Proxy (Api '[JWT])
proxyApi = Proxy

nt :: App -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

app :: Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> App -> Application
app cfg cs jwts ctx =
  serveWithContext proxyApi cfg $
    hoistServerWithContext proxyApi (Proxy :: Proxy [CookieSettings, JWTSettings])
      (flip runReaderT ctx) (apiServer cs jwts)