{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- I'm sorry Hubert
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Server where

import Data.Aeson.Types
import Data.Password (PasswordCheck (..))
import Data.Password (Password)
import Data.Password.Argon2 (checkPassword, hashPassword)
import Data.Password.Instances ()
import Database.Esqueleto.PostgreSQL.JSON (JSONB (..))
import Database.Persist.Postgresql (getEntity, insertBy, (=.), Entity (..), delete, fromSqlKey, get, getBy, insertEntity, toSqlKey, update)
import Import
import Models
import Network.HTTP.Types (status200)
import Network.Wai
import RIO.ByteString.Lazy (fromStrict, toStrict)
import qualified RIO.ByteString.Lazy as BL
import RIO.Text as T (pack)
import RIO.Time (UTCTime (..), fromGregorian)
import RIO.Partial (fromJust)
import Servant
import Servant.Auth.Docs ()
import Servant.Auth.Server (Auth, AuthResult (..), AuthResult, CookieSettings, FromJWT, JWT, JWTSettings, ToJWT, makeJWT, throwAll)
import Servant.Docs
import Util

-- | "Resource" types
data NewUserAccount = NewUserAccount
  { username :: Username,
    email :: Email,
    gender :: Maybe Gender,
    birthday :: Maybe UTCTime,
    location :: Maybe Text,
    zodiacSign :: Maybe ZodiacSign,
    password :: Password
  }
  deriving (Show, Generic)

instance FromJSON NewUserAccount

instance ToJSON NewUserAccount where
  toJSON NewUserAccount {..} =
    object
      [ "username" .= username,
        "email" .= email,
        "gender" .= gender,
        "birthday" .= birthday,
        "location" .= location,
        "zodiac_sign" .= zodiacSign,
        "password" .= ("somePassword" :: Text)
      ]

instance ToSample NewUserAccount where
  toSamples _ =
    singleSample $
      NewUserAccount
        "Paco.Alpaco"
        "paco@alpaca.net"
        (Just Male)
        (Just (UTCTime (fromGregorian 2017 2 14) 0))
        (Just "Shenzhen, China")
        (Just Capricorn)
        "secureAlpacaPassword"

data UpdateUserAccount = UpdateUserAccount
  { updateUsername :: Maybe Username,
    updateEmail :: Maybe Email,
    updateGender :: Maybe Gender,
    updateBirthday :: Maybe UTCTime,
    updateLocation :: Maybe Text,
    updateZodiacSign :: Maybe ZodiacSign
  }
  deriving (Show, Generic)

instance FromJSON UpdateUserAccount where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "update"}

instance ToJSON UpdateUserAccount where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = dropPrefix "update"}

instance ToSample UpdateUserAccount where
  toSamples _ =
    singleSample $
      UpdateUserAccount
        (Just "New.Alpaca.Name")
        (Just "new.email@alpaca.net")
        (Just NonBinary)
        Nothing
        Nothing
        Nothing

data UpdatePassword = UpdatePassword
  { currentPassword :: Password,
    newPassword :: Password
  }
  deriving (Show, Generic)

instance FromJSON UpdatePassword
instance ToJSON UpdatePassword where
  toJSON _ =
    object
      [ "currentPassword" .= ("sample" :: Text),
        "newPassword" .= ("anotherPassword" :: Text)
      ]


instance ToSample UpdatePassword where
  toSamples _ =
    singleSample $
      UpdatePassword "newPassword" "newPassword"

newtype UserId = UserId {userId :: Int64}
  deriving (Eq, Show, Read, Generic)

instance FromJSON UserId
instance ToJSON UserId

data AuthenticatedUser = AuthenticatedUser
  { auId :: UserId
    --, auEmail :: Text
  }
  deriving (Eq, Show, Read, Generic)


instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

instance ToSample AuthenticatedUser where
  toSamples _ = singleSample $ AuthenticatedUser $ UserId 42

data NewDream = NewDream
  { title :: Text,
    date :: UTCTime,
    description :: Text,
    emotions :: [EmotionLabel],
    lucid :: Bool,
    nightmare :: Bool,
    recurring :: Bool,
    private :: Bool,
    starred :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON NewDream
instance ToJSON NewDream

instance ToSample NewDream where
  toSamples _ =
    singleSample $
      NewDream
        "I dream of Alpacas"
        zeroTime
        "Some alpacas were wearing sunglasses"
        (map (fromJust . mkEmotionLabel) ["joy", "intimidated"])
        False
        False
        True
        False
        True

data DreamWithUserInfo = DreamWithUserInfo
  { dkTitle :: Text,
    dkDate :: UTCTime,
    dkDescription :: Text,
    dkEmotions :: [EmotionLabel],
    dkLucid :: Bool,
    dkNightmare :: Bool,
    dkRecurring :: Bool,
    dkPrivate :: Bool,
    dkStarred :: Bool,
    dkDreamerId :: Key UserAccount,
    dkDreamId :: Key Dream,
    dkDreamerUsername :: Username,
    dkDreamerLocation :: Maybe Text,
    dkDreamerGender :: Maybe Gender,
    dkDreamerZodiacSign :: Maybe ZodiacSign
  }
  deriving (Eq, Show, Generic)

instance FromJSON DreamWithUserInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "dk"}

instance ToJSON DreamWithUserInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = dropPrefix "dk"}

dreamWithKeys :: (Entity Dream, Entity UserAccount) -> DreamWithUserInfo
dreamWithKeys (Entity dreamId Dream {..}, Entity _ UserAccount {..}) =
  DreamWithUserInfo
    { dkTitle = dreamTitle,
      dkDate = dreamDreamedAt,
      dkDescription = dreamDescription,
      dkEmotions = (unJSONB $ dreamEmotions),
      dkLucid = dreamIsLucid,
      dkNightmare = dreamIsNightmare,
      dkRecurring = dreamIsRecurring,
      dkPrivate = dreamIsPrivate,
      dkStarred = dreamIsStarred,
      dkDreamerId = dreamUserId,
      dkDreamId = dreamId,
      dkDreamerUsername = userAccountUsername,
      dkDreamerLocation = userAccountLocation,
      dkDreamerGender = userAccountGender,
      dkDreamerZodiacSign = userAccountZodiacSign
    }

instance ToSample DreamWithUserInfo where
  toSamples _ =
    [("A dream with the dream id and dreamer id included", sampleDreamWithUserInfo)]
    where
      sampleDreamWithUserInfo =
        DreamWithUserInfo
          "I dreamed of our alpacas"
          zeroTime
          "Some alpacas were wearing sunglasses"
          (map (fromJust . mkEmotionLabel) ["joy", "intimidated"])
          False
          False
          True
          False
          True
          (toSqlKey 42)
          (toSqlKey 42)
          "alpaca.cool69420"
          (Just "Queens")
          (Just Female)
          (Just Scorpio)


data DreamUpdate = DreamUpdate
  { updateTitle :: Maybe Text,
    updateDate :: Maybe UTCTime,
    updateDescription :: Maybe Text,
    updateEmotions :: Maybe [EmotionLabel],
    updateLucid :: Maybe Bool,
    updateNightmare :: Maybe Bool,
    updateRecurring :: Maybe Bool,
    updatePrivate :: Maybe Bool,
    updateStarred :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON DreamUpdate where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "update"}

instance ToJSON DreamUpdate where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = dropPrefix "update"}

instance ToSample DreamUpdate where
  toSamples _ =
    [("All fields are optional; if emotions are sent, they will replace current ones.", sampleDreamUpdate)]
    where
      sampleDreamUpdate =
        DreamUpdate
          (Just "I dreamed a dream")
          Nothing
          Nothing
          (Just (map (fromJust . mkEmotionLabel) ["acceptance"]))
          (Just True)
          (Just False)
          (Just False)
          (Just True)
          (Just False)

data Login = Login
  { loginEmail :: Email,
    loginPassword :: Password
  }
  deriving (Show, Generic)

-- customize JSON instances: https://artyom.me/aeson#generics-handling-weird-field-names-in-data
instance FromJSON Login where
  -- drop the "login_" prefix, so we just need to say `email` and `password`
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "login"}

instance ToJSON Login where
  toJSON Login {..} =
    object
      [ "email" .= loginEmail,
        "password" .= ("somePassword" :: Text)
      ]

instance ToSample Login where
  toSamples _ =
    singleSample $
      Login "charlie@alpaca.net" "password"

data UserSession = UserSession
  { sessionToken :: Text,
    sessionUser :: UserAccount
  }
  deriving (Show, Generic)

instance ToJSON UserSession where
  -- drop the `session_` prefix, so we get `token` and `user`
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = dropPrefix "session"}

instance ToSample UserSession where
  toSamples _ =
    singleSample $
      UserSession "some-long-token" sampleUser

-- sad trombone: orphan instances. The servant docs totally expect this:
-- {-# OPTIONS_GHC -fno-warn-orphans #-}
instance ToCapture (Capture "dreamId" Int64) where
  toCapture _ = DocCapture "dreamId" "ID of the dream to update, as returned when creating it."

spiel :: String
spiel = " (if not provided, won't affect the filtering.)"
anyFlag :: [String]
anyFlag = ["true", "false"]

instance ToParam (QueryParam "lucid" Bool) where
  toParam _ =
    DocQueryParam "lucid" anyFlag ("Filter by: is lucid or not " <> spiel) Normal

instance ToParam (QueryParam "nightmare" Bool) where
  toParam _ =
    DocQueryParam "nightmare" anyFlag ("Filter by: is nightmare or not" <> spiel) Normal

instance ToParam (QueryParam "recurring" Bool) where
  toParam _ =
    DocQueryParam "recurring" anyFlag ("Filter by: is recurring or not" <> spiel) Normal

instance ToParam (QueryParams "emotions" EmotionLabel) where
  toParam _ =
    DocQueryParam "emotions" ["joy"] ("Filter by emotions: requires a list, will return dreams that have all the given emotions" <> spiel) List

instance ToParam (QueryParam "location" Text) where
  toParam _ =
    DocQueryParam "location" ["Queens"] ("Filter by location" <> spiel) Normal

instance ToParam (QueryParam "keywords" Text) where
  toParam _ =
    DocQueryParam "keywords" ["some cats are scary"] ("Filter by keyword, free text search." <> spiel) Normal

instance ToParam (QueryParam "gender" Gender) where
  toParam _ =
    DocQueryParam "gender" ["male", "female", "nonBinary"] ("Filter by dreamer's gender" <> spiel) Normal

instance ToParam (QueryParam "zodiac_sign" ZodiacSign) where
  toParam _ =
    DocQueryParam "zodiac_sign" ["capricorn", "sagittarius", "..."] ("Filter by dreamer's zodiac sign" <> spiel) Normal

instance ToParam (QueryParam "before" UTCTime) where
  toParam _ =
    DocQueryParam "before" ["2017-02-14T00:00:00Z"] ("Filter by dreamed at date: will returns any dreams before the given\
    \ date, inclusive." <> spiel) Normal

instance ToParam (QueryParam "after" UTCTime) where
  toParam _ =
    DocQueryParam "after" ["2017-02-14T00:00:00Z"] ("Filter by dreamed at date: will returns any dreams after the given\
    \ date, inclusive." <> spiel) Normal

instance ToParam (QueryParam "limit" Int64) where
  toParam _ =
    DocQueryParam "limit" ["101", "2", "..."] ("Limit the number of results. Defaults to 100 if not provided.") Normal

instance ToParam (QueryParam "last_seen_id" (Key Dream)) where
  toParam _ =
    DocQueryParam "last_seen_id" ["42"] ("The id of the last dream seen, for pagination. Omit for the first page.") Normal

instance ToParam (QueryParam "username" Username) where
  toParam _ =
    DocQueryParam "username" ["nena.alpaca"] ("A username. Checks existence. If you provide your own, we'll search private dreams too. If none is provide, search all public dreams.") Normal

instance ToParam (QueryFlag "mine") where
  toParam _ =
    DocQueryParam "mine" [] ("If specified, will only search the current user's dreams. Because this is a flag, you can call it like this: /api/dreams?mine") Flag

-- | API types
-- inspired by: https://github.com/haskell-servant/servant-auth/tree/696fab268e21f3d757b231f0987201b539c52621#readme

type Protected =
  "api" :> "user" :> Get '[JSON] UserAccount
    :<|> "api" :> "user" :> ReqBody '[JSON] UpdateUserAccount :> Verb 'PUT 204 '[JSON] NoContent
    :<|> "api" :> "user" :> "password" :> ReqBody '[JSON] UpdatePassword :> Verb 'PUT 204 '[JSON] NoContent
    :<|> "api" :> "user" :> "dreams" :> ReqBody '[JSON] NewDream :> PostCreated '[JSON] DreamWithUserInfo
    :<|> "api" :> "user" :> "dreams" :> Capture "dreamId" Int64 :> ReqBody '[JSON] DreamUpdate :> Verb 'PUT 204 '[JSON] NoContent
    :<|> "api" :> "user" :> "dreams" :> Capture "dreamId" Int64 :> Verb 'DELETE 204 '[JSON] NoContent

type Unprotected =
  "api" :> "users" :> ReqBody '[JSON] NewUserAccount :> PostCreated '[JSON] UserSession
    :<|> "api" :> "login" :> ReqBody '[JSON] Login :> PostCreated '[JSON] UserSession

type KindaProtected =
  "api" :> "dreams" :> 
    -- user filters
    QueryFlag "mine" :>
    QueryParam "username" Username :>
    QueryParam "location" Text :>
    QueryParam "gender" Gender :>
    QueryParam "zodiac_sign" ZodiacSign :>
    -- dream filters
    QueryParam "lucid" Bool :>
    QueryParam "nightmare" Bool :>
    QueryParam "recurring" Bool :>
    QueryParams "emotions" EmotionLabel :>
    QueryParam "keywords" Text :>
    -- date ranges
    QueryParam "before" UTCTime :>
    QueryParam "after" UTCTime :>
    -- pagination (TODO(luis) add first_seen_id for backwards pagination?)
    QueryParam "limit" Int64 :>
    QueryParam "last_seen_id" (Key Dream) :> Get '[JSON] [DreamWithUserInfo]


type Static =
  "docs" :> Raw

type Api auths =
  (Auth auths AuthenticatedUser :> Protected)
    :<|> Unprotected
    :<|> (Auth auths AuthenticatedUser :> KindaProtected)
    :<|> Static

type AppM = ReaderT App Servant.Handler

-- | Handlers
protected :: AuthResult AuthenticatedUser -> ServerT Protected AppM
protected (Authenticated authUser) =
  (currentUser authUser)
    :<|> (updateUser authUser)
    :<|> (updatePassword authUser)
    :<|> (createDream authUser)
    :<|> (updateDream authUser)
    :<|> (deleteDream authUser)

protected _ = throwAll err401

kindaProtected :: AuthResult AuthenticatedUser -> ServerT KindaProtected AppM
kindaProtected authResult = searchDreams authResult

unprotected :: CookieSettings -> JWTSettings -> ServerT Unprotected AppM
unprotected cs jwts = createUser cs jwts :<|> login cs jwts

-- Protected handlers

currentUser :: AuthenticatedUser -> AppM UserAccount
currentUser AuthenticatedUser {..} = do
  maybeUser <- runDB $ get $ toSqlKey $ userId auId
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just user -> return user

updateUser :: AuthenticatedUser -> UpdateUserAccount -> AppM NoContent
updateUser (AuthenticatedUser auId) UpdateUserAccount {..} = do
  maybeUser <- (runDB $ get $ ((toSqlKey (userId auId)) :: Key UserAccount))
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just _ -> do
      let updates =
            catMaybes $
              [ maybe Nothing (Just . (UserAccountUsername =.)) updateUsername,
                maybe Nothing (Just . (UserAccountEmail =.)) updateEmail,
                maybe Nothing (Just . (\x -> UserAccountGender =. Just x)) updateGender,
                maybe Nothing (Just . (\x -> UserAccountBirthday =. Just x)) updateBirthday,
                maybe Nothing (Just . (\x -> UserAccountLocation =. Just x)) updateLocation,
                maybe Nothing (Just . (\x -> UserAccountZodiacSign =. Just x)) updateZodiacSign
              ]
       in runDB $ update (toSqlKey $ userId auId) updates
      return NoContent

updatePassword :: AuthenticatedUser -> UpdatePassword -> AppM NoContent
updatePassword (AuthenticatedUser auId) UpdatePassword {..} = do
  maybeUser <- (runDB $ get $ ((toSqlKey (userId auId)) :: Key UserAccount))
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just user -> do
      case (checkPassword currentPassword (userAccountPassword user)) of
        PasswordCheckFail -> throwError $ err403 {errBody = "Unable to update password"}
        PasswordCheckSuccess -> do
          pwHash <- hashPassword newPassword
          runDB $ update (toSqlKey (userId auId)) [UserAccountPassword =. pwHash]
          return NoContent

createDream :: AuthenticatedUser -> NewDream -> AppM DreamWithUserInfo
createDream (AuthenticatedUser auId) NewDream {..} = do
  let userKey = toSqlKey $ userId auId :: Key UserAccount
      dbDream =
        Dream
          userKey
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

  me <- runDB $ getEntity userKey
  case me of
    Nothing -> throwError $ err404 {errBody = "Seems like I lost myself in my dreams!"}
    Just userEntity -> do
      dreamEntity <- runDB $ insertEntity dbDream
      return $ dreamWithKeys (dreamEntity, userEntity)

updateDream :: AuthenticatedUser -> Int64 -> DreamUpdate -> AppM NoContent
updateDream (AuthenticatedUser auId) dreamId updates = do
  let dreamKey = toSqlKey dreamId :: Key Dream
  let userKey = toSqlKey $ userId auId :: Key UserAccount
  maybeDream <- runDB $ get dreamKey
  case maybeDream of
    Nothing -> throwError $ err404 {errBody = "Dream not found."}
    Just existingDream -> do
      if (userKey == (dreamUserId existingDream))
        then updateDreamH dreamKey updates >> return NoContent
        else throwError $ err403 {errBody = "This is not your dream."}

deleteDream :: AuthenticatedUser -> Int64 -> AppM NoContent
deleteDream (AuthenticatedUser auId) dreamId = do
  let dreamKey = toSqlKey dreamId :: Key Dream
  let userKey = toSqlKey $ userId auId :: Key UserAccount
  maybeDream <- runDB $ get dreamKey
  case maybeDream of
    Nothing -> throwError $ err410 {errBody = "The dream is gone."}
    Just existingDream -> do
      if (userKey == (dreamUserId existingDream))
        then runDB (delete dreamKey) >> return NoContent
        else throwError $ err403 {errBody = "This is not your dream."}

-- Unprotected handlers

createUser :: CookieSettings -> JWTSettings -> NewUserAccount -> AppM UserSession
createUser _ jwts NewUserAccount {..} = do
  hashedPw <- hashPassword password
  let newUser = UserAccount email
                  hashedPw
                  username
                  gender
                  birthday
                  location
                  zodiacSign
                  zeroTime -- createdAt is set by a database trigger, so we use a bogus timestamp.
                  zeroTime -- updatedAt is also set by a db trigger.
  maybeNewUserId <- runDB $ insertBy newUser
  case maybeNewUserId of
    Left (Entity _ existingUser) -> throwError $ err409 {errBody = uniqueUserError newUser existingUser}
    Right newUserId -> sessionWithUser jwts newUserId

uniqueUserError :: UserAccount -> UserAccount -> BL.ByteString
uniqueUserError newUser existingUser
  | (userAccountUsername newUser) == (userAccountUsername existingUser) = "Unable to create user: username is already taken."
  | (userAccountEmail    newUser) == (userAccountEmail existingUser) = "Unable to create user: email is already taken."
  | otherwise = "Unable to create user: duplicate user"

login :: CookieSettings -> JWTSettings -> Login -> AppM UserSession
login _ jwts Login {..} = do
  maybeUser <- runDB $ getBy $ UniqueEmail loginEmail
  case maybeUser of
    Nothing -> throwError $ err401 {errBody = "Invalid email or password."}
    Just (Entity userId user) -> do
      case (checkPassword loginPassword (userAccountPassword user)) of
        PasswordCheckFail -> throwError $ err401 {errBody = "Invalid email or password."}
        PasswordCheckSuccess -> sessionWithUser jwts userId

-- Handlers that check their own authentication

searchDreams :: AuthResult AuthenticatedUser
  -> Bool -- only mine?
  -> Maybe Username
  -> Maybe Text -- location
  -> Maybe Gender
  -> Maybe ZodiacSign
  -> Maybe Bool -- lucid
  -> Maybe Bool -- nightmare
  -> Maybe Bool -- recurring
  -> [EmotionLabel] 
  -> Maybe Text -- keywords
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Maybe Int64
  -> Maybe (Key Dream)
  -> AppM [DreamWithUserInfo]

-- if the "mine" flag is specified, search only my dreams. Convenience to not have to provide my own username.
searchDreams (Authenticated (AuthenticatedUser auId)) True _ t g z l n r es k b a lt ls =
  let currentUserId = toSqlKey $ userId $ auId
  in
    searchDreams' (Just (currentUserId, True)) t g z l n r es k b a lt ls

searchDreams _ True _ t g z l n r es k b a lt ls =
  throwError $ err401 {errBody= "Need to be signed in to search your own dreams!"}

-- searching a user's dreams: if I provide my own username, search my dreams. If I provide someone else's,
-- search their _public_ dreams.
searchDreams (Authenticated (AuthenticatedUser auId)) _ (Just username) t g z l n r es k b a lt ls = do
  requestedUser <- runDB $ getBy $ UniqueUsername username
  let currentUserId = toSqlKey $ userId $ auId
  case requestedUser of
    Nothing -> throwError $ err404 {errBody =  "The requested user is not a known dreamer."}
    Just (Entity requestedUserId _) -> 
      if (currentUserId == requestedUserId) then
        searchDreams' (Just (requestedUserId, True)) t g z l n r es k b a lt ls
      else
        searchDreams' (Just (requestedUserId, False)) t g z l n r es k b a lt ls

-- not authenticated (or failed authentication,) but searching a specific user's dreams:
-- always search as a non-owner
searchDreams _ _ (Just username) t g z l n r es k b a lt ls = do
  requestedUser <- runDB $ getBy $ UniqueUsername username
  case requestedUser of
    Nothing -> throwError $ err404 {errBody = "The requested user is not a known dreamer."}
    Just (Entity requestedUserId _) ->
      searchDreams' (Just (requestedUserId, False)) t g z l n r es k b a lt ls

-- not searching by username: we're just searching public dreams for everyone.
searchDreams _ _ Nothing t g z l n r es k b a lt ls =
  searchDreams' Nothing t g z l n r es k b a lt ls

searchDreams' :: Maybe (Key UserAccount, Bool)
  -> Maybe Text -- location
  -> Maybe Gender 
  -> Maybe ZodiacSign 
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool 
  -> [EmotionLabel] 
  -> Maybe Text -- keywords 
  -> Maybe UTCTime 
  -> Maybe UTCTime 
  -> Maybe Int64 
  -> Maybe (Key Dream) 
  -> AppM [DreamWithUserInfo]
searchDreams' userFilters location gender zodiacSign lucid nightmare recurring es keywords before after limit lastSeen = do
  let emotions = if null es then Nothing else Just es
      filters = DreamFilters 
        {
          filterLocation = location,
          filterGender = gender,
          filterZodiacSign = zodiacSign,
          filterLucid = lucid,
          filterNightmare = nightmare,
          filterRecurring = recurring,
          filterEmotions = emotions,
          filterKeyword = keywords,
          filterBefore = before,
          filterAfter = after,
          filterLimit = limit,
          filterLastSeenId = lastSeen
        }
  dreams <- runDB $ filteredDreams filters userFilters
  return $ map dreamWithKeys dreams

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

updateDreamH :: (MonadReader s m, HasDBConnectionPool s, MonadIO m) => Key Dream -> DreamUpdate -> m ()
updateDreamH dreamKey DreamUpdate {..} =
  let updates =
        catMaybes $
          [ maybe Nothing (Just . (DreamTitle =.)) updateTitle,
            maybe Nothing (Just . (DreamDreamedAt =.)) updateDate,
            maybe Nothing (Just . (DreamDescription =.)) updateDescription,
            maybe Nothing (Just . (DreamIsLucid =.)) updateLucid,
            maybe Nothing (Just . (DreamIsNightmare =.)) updateNightmare,
            maybe Nothing (Just . (DreamIsRecurring =.)) updateRecurring,
            maybe Nothing (Just . (DreamIsPrivate =.)) updatePrivate,
            maybe Nothing (Just . (DreamIsStarred =.)) updateStarred,
            maybe Nothing (Just . (\e -> DreamEmotions =. JSONB e)) updateEmotions
          ]
   in runDB $ update dreamKey updates

-- | Server construction
docsH :: Tagged AppM (p -> (Network.Wai.Response -> t) -> t)
docsH = return serveDocs
  where
    serveDocs _ respond =
      respond $ responseLBS status200 [plain] (fromStrict docsBs)
    plain = ("Content-Type", "text/plain")
    docsBs =
      encodeUtf8
        . T.pack
        . markdown
        $ docsWithIntros [intro] proxyApi
    intro =
      DocIntro
        "Undercurrent API"
        [ "For an up-to-date version of this documentation, visit the `/docs` endpoint of the API.\
          \For all `JWT` protected endpoints, you must provide it in the `Authorization` header, with a value of `Bearer THE_TOKEN`\
          \(where `THE_TOKEN` is what's returned in the `token` property after logging in or creating a user.)"
        ]

apiServer :: CookieSettings -> JWTSettings -> ServerT (Api auths) AppM
apiServer cs jwts =
  protected
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
    hoistServerWithContext
      proxyApi
      (Proxy :: Proxy [CookieSettings, JWTSettings])
      (flip runReaderT ctx)
      (apiServer cs jwts)
