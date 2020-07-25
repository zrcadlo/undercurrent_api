{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings#-}
module Models where

import           Import
import           Database.Persist.TH            ( share
                                                , mkPersist
                                                , sqlSettings
                                                , mkMigrate
                                                , persistLowerCase
                                                )
import           RIO.Time                       ( UTCTime )
import           Database.Persist.Postgresql    (Key
                                                , Entity(..)
                                                , SqlPersistT
                                                , rawExecute
                                                , runSqlPool
                                                
                                                , SqlBackend
                                                )
import           Data.Password                  ( PasswordHash(..) )
import           Data.Password.Instances        ( )
import           Data.Password.Argon2           ( Argon2 )
import           Servant.Docs
import           Data.Aeson.Types
import qualified Database.Esqueleto            as E
import qualified Database.Esqueleto.PostgreSQL.JSON as E
-- uncomment for special operators like arrayAgg
--import qualified Database.Esqueleto.PostgreSQL as E
import           Database.Esqueleto.PostgreSQL.JSON
                                                ( JSONB )
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp, unsafeSqlFunction)
import Util (zeroTime)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    UserAccount
        email Email
        password (PasswordHash Argon2)
        username Username
        gender Gender Maybe
        birthday UTCTime Maybe
        location Text Maybe
        zodiacSign ZodiacSign Maybe
        createdAt UTCTime default=now()
        updatedAt UTCTime default=now()
        UniqueEmail email
        UniqueUsername username
        deriving Show

    Dream
        userId UserAccountId
        title Text
        description Text
        isLucid Bool
        isNightmare Bool
        isRecurring Bool
        isPrivate Bool
        isStarred Bool
        emotions  (JSONB [EmotionLabel])
        dreamedAt UTCTime default=now()
        createdAt UTCTime default=now()
        updatedAt UTCTime default=now()
        deriving Eq Show
|]

-- manually rolling out the JSON instance for UserAccount to avoid exposing the password.
-- cf.:
-- https://www.reddit.com/r/haskell/comments/9q2plk/persistent_foreign_keys_json/e86b3vm/
-- https://github.com/yesodweb/persistent/pull/181/files (not used, but interesting)
-- https://artyom.me/aeson
instance ToJSON UserAccount where
    toJSON UserAccount{..} = object
        [ "email" .= userAccountEmail
        , "username" .= userAccountUsername
        , "gender" .= userAccountGender
        , "birthday" .= userAccountBirthday
        , "location" .= userAccountLocation
        , "zodiac_sign" .= userAccountZodiacSign
        -- TODO(luis) maybe surface created_at?
        ]

runDB
    :: (MonadReader s m, HasDBConnectionPool s, MonadIO m)
    => ReaderT SqlBackend IO b
    -> m b
runDB query = do
    pool <- view dbConnectionPoolL
    liftIO $ runSqlPool query pool


-- | Custom operators and functions
-- as per: https://github.com/bitemyapp/esqueleto/tree/4dbd5339adf99e1f045c0a02211a03c79032f9cf#unsafe-functions-operators-and-values
tsVector :: E.SqlExpr (E.Value s) -> E.SqlExpr (E.Value s)
tsVector v = unsafeSqlFunction "to_tsvector" v

{-| Generate a standardize tsquery based on unsanitized input.
    not using to_tsquery since innocent strings like `"fear the cat"`
    would break it (it expects them to be e.g. `"fear & cat"`)
    fortunately, `websearch_to_tsquery` does all that for us:
    https://www.postgresql.org/docs/current/textsearch-controls.html#TEXTSEARCH-PARSING-QUERIES

    * unquoted text: text not inside quote marks will be converted to terms separated by & operators, as if processed by plainto_tsquery.
    * "quoted text": text inside quote marks will be converted to terms separated by <-> operators, as if processed by phraseto_tsquery.
    * OR: logical or will be converted to the | operator.
    * -: the logical not operator, converted to the the ! operator.
-}
webTsQuery :: E.SqlExpr (E.Value Text) -> E.SqlExpr (E.Value Text)
webTsQuery q = unsafeSqlFunction "websearch_to_tsquery" q

-- from:
-- https://github.com/bitemyapp/esqueleto/pull/119/files

(@@.) :: E.SqlExpr (E.Value s) -> E.SqlExpr (E.Value s) -> E.SqlExpr (E.Value Bool)
(@@.) = unsafeSqlBinOp " @@ "

-- | Queries

-- TODO: actually need a buncha more filters:
-- for reference:
-- https://gist.github.com/bitemyapp/89c5e0663bddc3b1c78f5e3fa33e7dc4#file-companiescount-hs-L163
-- https://github.com/bitemyapp/esqueleto/blob/master/test/PostgreSQL/Test.hs#L1116
-- 
data DreamFilters = DreamFilters
    { filterLucid :: Maybe Bool
    , filterNightmare :: Maybe Bool
    , filterRecurring :: Maybe Bool
    , filterEmotions :: Maybe [EmotionLabel]
    , filterKeyword :: Maybe Text
    -- user properties
    , filterLocation :: Maybe Text
    , filterGender :: Maybe Gender
    , filterZodiacSign :: Maybe ZodiacSign
    -- range properties
    , filterBefore :: Maybe UTCTime
    , filterAfter :: Maybe UTCTime  
    -- pagination properties
    , filterLimit :: Maybe Int64
    , filterLastSeenId :: Maybe (Key Dream)
    } deriving (Eq, Show, Generic)

instance FromJSON DreamFilters
instance ToJSON DreamFilters
instance ToSample DreamFilters where
    toSamples _ = 
        [("Filters to search dreams by, all fields are optional. Limit defaults to 100", sampleFilters)]
        where
            sampleFilters = DreamFilters (Just True)
                                         Nothing
                                        (Just False)
                                        (Just [EmotionLabel "joy"])
                                        (Just "alpacas")
                                        (Just "Tokyo, Japan")
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        (Just 200)
                                        Nothing

noDreamFilters :: DreamFilters
noDreamFilters = DreamFilters
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

filteredDreams
    :: (MonadIO m)
    => DreamFilters
    -> (Maybe (Key UserAccount, Bool))
    -> ReaderT SqlBackend m [(Entity Dream, Entity UserAccount)]
filteredDreams DreamFilters{..} userConditions = do
    let maybeNoConditions = maybe (return ())
    E.select . E.from $ \(dream `E.InnerJoin` userAccount) -> do
        -- Ownership filters
        E.on (userAccount E.^. UserAccountId E.==. dream E.^. DreamUserId)
        maybe
            -- if no user conditions, simply look through public dreams
            (E.where_ (dream E.^. DreamIsPrivate E.==. E.val False))
            -- if there are, check if the given user is the owner!
            (\(userId, isOwner) ->
                if isOwner then
                    E.where_
                        (     dream
                        E.^.  DreamUserId
                        E.==. (E.val userId)
                        )
                else
                    E.where_
                        (     dream
                        E.^.  DreamUserId
                        E.==. (E.val userId)
                        E.&&. dream
                        E.^.  DreamIsPrivate
                        E.==. (E.val False)
                    )
            )
            userConditions
        -- inspired by: https://gist.github.com/bitemyapp/89c5e0663bddc3b1c78f5e3fa33e7dc4#file-companiescount-hs-L79
        -- Dream Filters
        maybeNoConditions (\l -> E.where_ (dream E.^. DreamIsLucid     E.==. E.val l)) filterLucid
        maybeNoConditions (\n -> E.where_ (dream E.^. DreamIsNightmare E.==. E.val n)) filterNightmare
        maybeNoConditions (\r -> E.where_ (dream E.^. DreamIsRecurring E.==. E.val r)) filterRecurring
        maybeNoConditions (\es -> E.where_ (E.just (dream E.^. DreamEmotions) E.@>. (E.jsonbVal es))) filterEmotions
        -- TODO: need an index!
        -- more info on full text queries:
        -- https://www.postgresql.org/docs/current/textsearch-tables.html
        maybeNoConditions
            (\kw ->
                E.where_
                    $ (tsVector $ dream E.^. DreamTitle E.++. (E.val " ") E.++. dream E.^. DreamDescription)
                    @@.
                    (webTsQuery $ E.val kw)
            )
            filterKeyword
        -- User Account filters
        maybeNoConditions (\b -> E.where_ (userAccount E.^. UserAccountLocation E.==. (E.just $ E.val b))) filterLocation
        maybeNoConditions (\g -> E.where_ (userAccount E.^. UserAccountGender E.==. (E.just $ E.val g))) filterGender
        maybeNoConditions (\z -> E.where_ (userAccount E.^. UserAccountZodiacSign E.==. (E.just $ E.val z))) filterZodiacSign
        -- Ranges
        maybeNoConditions (\before -> E.where_ (dream E.^. DreamDreamedAt E.<=. E.val before)) filterBefore
        maybeNoConditions (\after -> E.where_ (dream E.^. DreamDreamedAt  E.>=. E.val after)) filterAfter
        -- keyset pagination
        maybeNoConditions (\lastSeen -> E.where_(dream E.^. DreamId E.<. E.val lastSeen)) filterLastSeenId
        -- default page size is 200, max is 1000.
        E.limit $ maybe 200 (\l-> if l > 1000 then 1000 else l) filterLimit
        E.orderBy [ E.desc (dream E.^. DreamId) ]
        return (dream, userAccount)

userDreams
    :: (MonadReader s m, HasDBConnectionPool s, MonadIO m)
    => Key UserAccount
    -> Bool
    -> m [(Entity Dream, Entity UserAccount)]
userDreams u o = runDB $ filteredDreams noDreamFilters $ Just (u, o)


-- | Documentation helpers

-- TODO: the day approaches where it's obvious we need an "application" version of UserAccount.
sampleUser :: UserAccount
sampleUser = UserAccount "nena@alpaca.com"
                         (PasswordHash "secureAlpacaPassword")
                         "nena.alpaca"
                         (Just Female)
                         Nothing
                         (Just "Tokyo, Japan")
                         (Just Scorpio)
                         zeroTime
                         zeroTime

instance ToSample UserAccount where
    toSamples _ = singleSample sampleUser

-- | Concessions to testing

dropModels :: (MonadIO m) => SqlPersistT m ()
dropModels = rawExecute
    "TRUNCATE TABLE user_account, dream RESTART IDENTITY"
    []
