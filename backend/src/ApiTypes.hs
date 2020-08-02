{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- I'm sorry Hubert
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ApiTypes where

import Data.Aeson.Types
import Data.Password (Password)
import Data.Password.Instances ()
import Database.Persist.Sql (toSqlKey)
import Import
import Models
import RIO.Partial (fromJust)
import RIO.Time (UTCTime (..), fromGregorian)
import Servant
import Servant.Auth.Docs ()
import Servant.Docs
import Servant.Auth.Server (Auth, FromJWT, ToJWT)
import Util

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
  "api" :> "dreams"
    :>
    -- user filters
    QueryFlag "mine"
    :> QueryParam "username" Username
    :> QueryParam "location" Text
    :> QueryParam "gender" Gender
    :> QueryParam "zodiac_sign" ZodiacSign
    :>
    -- dream filters
    QueryParam "lucid" Bool
    :> QueryParam "nightmare" Bool
    :> QueryParam "recurring" Bool
    :> QueryParams "emotions" EmotionLabel
    :> QueryParam "keywords" Text
    :>
    -- date ranges
    QueryParam "before" UTCTime
    :> QueryParam "after" UTCTime
    :>
    -- pagination (TODO(luis) add first_seen_id for backwards pagination?)
    QueryParam "limit" Int64
    :> QueryParam "last_seen_id" (Key Dream)
    :> Get '[JSON] [DreamWithUserInfo]
  :<|> "api" :> "stats"
    :>
    QueryFlag "mine"
    :> QueryParam "username" Username
    :> QueryParam "location" Text
    :> QueryParam "gender" Gender
    :> QueryParam "zodiac_sign" ZodiacSign
    :>
    -- dream filters
    QueryParam "lucid" Bool
    :> QueryParam "nightmare" Bool
    :> QueryParam "recurring" Bool
    :> QueryParams "emotions" EmotionLabel
    :> QueryParam "keywords" Text
    :>
    -- date ranges
    QueryParam "before" UTCTime
    :> QueryParam "after" UTCTime
    :>
    -- top words and emotions
    QueryParam "top" Int
    :> Get '[JSON] DreamStats

type Static =
  "docs" :> Raw

type Api auths =
  (Auth auths AuthenticatedUser :> Protected)
    :<|> Unprotected
    :<|> (Auth auths AuthenticatedUser :> KindaProtected)
    :<|> Static

type AppM = ReaderT App Servant.Handler

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
          "alpaca.cool69420"
          (Just "Queens")
          (Just Female)
          (Just Scorpio)

newtype KeywordStats = KeywordStats {unKeywordStats :: KeywordStatsDB}
  deriving (Eq, Show, Generic)
instance ToJSON KeywordStats where
  toJSON = unKeywordStats >>> genericToJSON defaultOptions

newtype EmotionStats = EmotionStats {unEmotionStats :: EmotionStatsDB}
  deriving (Eq, Show, Generic)
instance ToJSON EmotionStats where
  toJSON = unEmotionStats >>> genericToJSON defaultOptions {fieldLabelModifier = dropPrefix "emotion"}

data DreamStats = DreamStats
  {
    topKeywords :: [KeywordStats]
  , topEmotions :: [EmotionStats]
  }
  deriving (Eq, Show, Generic)

instance ToJSON DreamStats

instance ToSample DreamStats where
  toSamples _ =
    [("Totals represent the sample taken, there could be more!", sampleStats)]
    where
      sampleStats =
        DreamStats {
          topKeywords = [KeywordStats $ KeywordStatsDB "winter" 1 0 0 1 "sadness"]
        , topEmotions = [EmotionStats $ EmotionStatsDB (EmotionLabel "sadness") 1 0 0 1]
        }


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
    DocQueryParam
      "before"
      ["2017-02-14T00:00:00Z"]
      ( "Filter by dreamed at date: will returns any dreams before the given\
        \ date, inclusive."
          <> spiel
      )
      Normal

instance ToParam (QueryParam "after" UTCTime) where
  toParam _ =
    DocQueryParam
      "after"
      ["2017-02-14T00:00:00Z"]
      ( "Filter by dreamed at date: will returns any dreams after the given\
        \ date, inclusive."
          <> spiel
      )
      Normal

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


instance ToParam (QueryParam "top" Int) where
  toParam _ =
    DocQueryParam "top" ["10", "100"] ("Get the top N keywords and emotions. Returns the top 10 if unspecified, max is 100.") Normal
