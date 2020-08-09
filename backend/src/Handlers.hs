{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import ApiTypes
import Data.Password (PasswordCheck (..))
import Data.Password.Argon2 (checkPassword, hashPassword)
import Data.Password.Instances ()
import Database.Esqueleto.PostgreSQL.JSON (JSONB (..))
import Database.Persist.Postgresql ((=.), Entity (..), delete, fromSqlKey, get, getBy, getEntity, insertBy, insertEntity, toSqlKey, update)
import Import
import Models
import RIO.ByteString.Lazy (toStrict)
import qualified RIO.ByteString.Lazy as BL
import RIO.Time (UTCTime (..))
import Servant
import Servant.Auth.Docs ()
import Servant.Auth.Server (AuthResult (..), AuthResult, CookieSettings, JWTSettings, makeJWT, throwAll)
import Util

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
kindaProtected authResult = 
  (searchDreams authResult)
    :<|> (dreamStats authResult)

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
      let parsedLocation = parseLocation <$> updateLocation
          updates =
            catMaybes $
              [ maybe Nothing (Just . (UserAccountUsername =.)) updateUsername,
                maybe Nothing (Just . (UserAccountEmail =.)) updateEmail,
                maybe Nothing (Just . (\x -> UserAccountGender =. Just x)) updateGender,
                maybe Nothing (Just . (\x -> UserAccountBirthday =. Just x)) updateBirthday,
                maybe Nothing (Just . (\x -> UserAccountLocation =. (Just . JSONB) x)) parsedLocation,
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
      parsedLocation = parseLocation <$> dreamerLocation
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
          (maybe Nothing (Just . JSONB) parsedLocation)

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
  let userLocation = parseLocation <$> location
      newUser =
        UserAccount
          email
          hashedPw
          username
          gender
          birthday
          (maybe Nothing (Just . JSONB) userLocation)
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
  | (userAccountEmail newUser) == (userAccountEmail existingUser) = "Unable to create user: email is already taken."
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

privateDreamsFor :: (Key UserAccount) -> Maybe (Key UserAccount, Bool)
privateDreamsFor u = Just (u, True)

publicDreamsFor :: (Key UserAccount) -> Maybe (Key UserAccount, Bool)
publicDreamsFor u = Just (u, False)
searchDreams ::
  AuthResult AuthenticatedUser ->
  Bool -> -- only mine?
  Maybe Username ->
  Maybe Text -> -- location
  Maybe Gender ->
  Maybe ZodiacSign ->
  Maybe Bool -> -- lucid
  Maybe Bool -> -- nightmare
  Maybe Bool -> -- recurring
  [EmotionLabel] ->
  Maybe Text -> -- keywords
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int64 ->
  Maybe (Key Dream) ->
  AppM [DreamWithUserInfo]

-- if the "mine" flag is specified, search only my dreams. Convenience to not have to provide my own username.
searchDreams (Authenticated user) True _ t g z l n r es k b a lt ls =
  let currentUserId = toSqlKey $ userId $ auId user
   in searchDreams' (privateDreamsFor currentUserId) t g z l n r es k b a lt ls
-- asking for "mine," but there is no me.
searchDreams _ True _ _ _ _ _ _ _ _ _ _ _ _ _ =
  throwError $ err401 {errBody = "Need to be signed in to search your own dreams!"}
-- not searching by username: we're just searching public dreams for everyone.
searchDreams _ _ Nothing t g z l n r es k b a lt ls =
  searchDreams' Nothing t g z l n r es k b a lt ls
-- searching a user's dreams: if I provide my own username, search my dreams. If I provide someone else's,
-- search their _public_ dreams.
-- TODO(luis) maybe this should behave exactly as the non-authenticated version? Even if I provide
-- my own username, only search my public dreams? It's easy to update, just delete this match!
searchDreams (Authenticated user) _ (Just username) t g z l n r es k b a lt ls = do
  requestedUser <- runDB $ getBy $ UniqueUsername username
  let currentUserId = toSqlKey $ userId $ auId user
  case requestedUser of
    Nothing -> throwError $ err404 {errBody = "The requested user is not a known dreamer."}
    Just (Entity requestedUserId _) ->
      if (currentUserId == requestedUserId)
        then searchDreams' (privateDreamsFor requestedUserId) t g z l n r es k b a lt ls
        else searchDreams' (publicDreamsFor requestedUserId) t g z l n r es k b a lt ls

-- not authenticated (or failed authentication,) but searching a specific user's dreams:
-- always search as a non-owner
searchDreams _ _ (Just username) t g z l n r es k b a lt ls = do
  requestedUser <- runDB $ getBy $ UniqueUsername username
  case requestedUser of
    Nothing -> throwError $ err404 {errBody = "The requested user is not a known dreamer."}
    Just (Entity requestedUserId _) ->
      searchDreams' (publicDreamsFor requestedUserId) t g z l n r es k b a lt ls

searchDreams' ::
  Maybe (Key UserAccount, Bool) ->
  Maybe Text -> -- location
  Maybe Gender ->
  Maybe ZodiacSign ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  [EmotionLabel] ->
  Maybe Text -> -- keywords
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int64 ->
  Maybe (Key Dream) ->
  AppM [DreamWithUserInfo]
searchDreams' userFilters location gender zodiacSign lucid nightmare recurring es keywords before after limit lastSeen = do
  let emotions = if null es then Nothing else Just es
      filters =
        DreamFilters
          { filterLocation = location,
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

-- TODO(luis) this endpoint is so painfully duplicative of searchDreams!!
dreamStats ::
  AuthResult AuthenticatedUser ->
  Bool -> -- only mine?
  Maybe Username ->
  Maybe Text -> -- location
  Maybe Gender ->
  Maybe ZodiacSign ->
  Maybe Bool -> -- lucid
  Maybe Bool -> -- nightmare
  Maybe Bool -> -- recurring
  [EmotionLabel] ->
  Maybe Text -> -- keywords
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int -> -- top N words and emotions?
  AppM DreamStats

dreamStats (Authenticated user) True _ loc g z l n r es k b a lt =
  let currentUserId = toSqlKey $ userId $ auId user
    in dreamStats' (privateDreamsFor currentUserId) loc g z l n r es k b a lt

dreamStats _ True _ _ _ _ _ _ _ _ _ _ _ _ =
  throwError $ err401 {errBody = "Need to be signed in to get your dreams' statistics!"}

dreamStats _ _ Nothing loc g z l n r es k b a lt =
  dreamStats' Nothing loc g z l n r es k b a lt

dreamStats (Authenticated user) _ (Just username) loc g z l n r es k b a lt = do
  requestedUser <- runDB $ getBy $ UniqueUsername username
  let currentUserId = toSqlKey $ userId $ auId user
  case requestedUser of
    Nothing -> throwError $ err404 {errBody = "The requested user is not a known dreamer."}
    Just (Entity requestedUserId _) ->
      if (currentUserId == requestedUserId)
        then dreamStats' (privateDreamsFor requestedUserId) loc g z l n r es k b a lt
        else dreamStats' (publicDreamsFor requestedUserId) loc g z l n r es k b a lt

dreamStats _ _ (Just username) loc g z l n r es k b a lt = do
  requestedUser <- runDB $ getBy $ UniqueUsername username
  case requestedUser of
    Nothing -> throwError $ err404 {errBody = "The requested user is not a known dreamer."}
    Just (Entity requestedUserId _) ->
      dreamStats' (publicDreamsFor requestedUserId) loc g z l n r es k b a lt

dreamStats' ::
  OwnerFilters ->
  Maybe Text -> -- location
  Maybe Gender ->
  Maybe ZodiacSign ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  [EmotionLabel] ->
  Maybe Text -> -- keywords
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int -> -- top N words/emotions
  AppM DreamStats
dreamStats' userFilters location gender zodiacSign lucid nightmare recurring es keywords before after maybeN = do
  let emotions = if null es then Nothing else Just es
      topN = maybe 10 (\t -> if t > 100 then 100 else t) maybeN
      filters =
        DreamFilters
          { filterLocation = location,
            filterGender = gender,
            filterZodiacSign = zodiacSign,
            filterLucid = lucid,
            filterNightmare = nightmare,
            filterRecurring = recurring,
            filterEmotions = emotions,
            filterKeyword = keywords,
            filterBefore = before,
            filterAfter = after,
            filterLimit = Nothing,
            filterLastSeenId = Nothing
          }      
  topKeywords <- runDB $ keywordStats topN filters userFilters
  topEmotions <- runDB $ emotionStats topN filters userFilters
  return $ DreamStats (map KeywordStats topKeywords) (map EmotionStats topEmotions)

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
  let parsedLocation = parseLocation <$> updateDreamerLocation
      updates =
        catMaybes $
          [ maybe Nothing (Just . (DreamTitle =.)) updateTitle,
            maybe Nothing (Just . (DreamDreamedAt =.)) updateDate,
            maybe Nothing (Just . (DreamDescription =.)) updateDescription,
            maybe Nothing (Just . (DreamIsLucid =.)) updateLucid,
            maybe Nothing (Just . (DreamIsNightmare =.)) updateNightmare,
            maybe Nothing (Just . (DreamIsRecurring =.)) updateRecurring,
            maybe Nothing (Just . (DreamIsPrivate =.)) updatePrivate,
            maybe Nothing (Just . (DreamIsStarred =.)) updateStarred,
            maybe Nothing (Just . (\e -> DreamEmotions =. JSONB e)) updateEmotions,
            maybe Nothing (Just . (\l -> DreamLocation =. (Just . JSONB) l)) parsedLocation
          ]
   in runDB $ update dreamKey updates

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
      dkDreamId = dreamId,
      dkDreamerUsername = userAccountUsername,
      dkDreamerLocation = (unJSONB <$> dreamLocation),
      dkDreamerGender = userAccountGender,
      dkDreamerZodiacSign = userAccountZodiacSign
    }

parseLocation :: APILocation -> Location
parseLocation APILocation{..} =
  Location parsedCity parsedRegion parsedCountry parsedLat parsedLng
  where
    parsedCity = case algoliaType of
        Just "city" -> algoliaName
        _ -> algoliaCity
    parsedRegion = algoliaAdministrative
    parsedCountry = algoliaCountry
    parsedLat = algoliaLatlng >>= Just . lat
    parsedLng = algoliaLatlng >>= Just . lng
