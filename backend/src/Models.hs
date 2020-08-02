{-# LANGUAGE RankNTypes #-}
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
import           Database.Persist.Postgresql    (withPostgresqlConn, ConnectionString, Key
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
import Database.Esqueleto.Internal.Sql (unsafeSqlValue, unsafeSqlBinOp, unsafeSqlFunction)
import Util (zeroTime)
import Database.Persist.Sql.Raw.QQ (sqlQQ)
import Database.Persist.Sql (Single(..))
import Database.Persist.Postgresql (runSqlConn)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Esqueleto (SqlQuery)

type DBM m a = (MonadIO m) => ReaderT SqlBackend m a
type QueryM a = forall m. DBM m a

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

runDBSimple
    :: (MonadUnliftIO m)
    => ConnectionString
    -> ReaderT SqlBackend (LoggingT m) b
    -> m b
runDBSimple conStr query = 
    runStdoutLoggingT $ withPostgresqlConn conStr $ runSqlConn query


-- | Default dictionary for all text searches. We currently support English only,
-- but may create a different one if necessary. Note that we also have `english_simple`
-- for statistics (see indexes at 202007271200_text_search_config.sql)
defaultTextSearchDictionary :: E.SqlExpr (E.Value String)
defaultTextSearchDictionary = unsafeSqlValue "\'english\'"

-- | Custom operators and functions
-- as per: https://github.com/bitemyapp/esqueleto/tree/4dbd5339adf99e1f045c0a02211a03c79032f9cf#unsafe-functions-operators-and-values
tsVector :: E.SqlExpr (E.Value s) -> E.SqlExpr (E.Value s)
tsVector v = unsafeSqlFunction "to_tsvector" (defaultTextSearchDictionary, v)


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
webTsQuery q = unsafeSqlFunction "websearch_to_tsquery" (defaultTextSearchDictionary, q)

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

type OwnerFilters = (Maybe (Key UserAccount, Bool))

filteredDreams
    :: DreamFilters
    -> OwnerFilters
    -> QueryM [(Entity Dream, Entity UserAccount)]
filteredDreams dreamFilters userConditions = do
    E.select . E.from $ \(dream `E.InnerJoin` userAccount) -> do
        E.on (userAccount E.^. UserAccountId E.==. dream E.^. DreamUserId)
        restrictByOwnerConditions dream userConditions
        restrictByDreamFilters dream userAccount dreamFilters
        -- PAGINATION
        -- default page size is 100, max is 1000.
        maybeNoConditions (\lastSeen -> E.where_(dream E.^. DreamId E.<. E.val lastSeen)) (filterLastSeenId dreamFilters)
        E.limit $ maybe 100 (\l-> if l > 1000 then 1000 else l) (filterLimit dreamFilters)
        E.orderBy [ E.desc (dream E.^. DreamId) ]
        return (dream, userAccount)

sampleDreamIds
    :: Int64
    -> DreamFilters
    -> OwnerFilters
    -> QueryM [E.Value (Key Dream)]
sampleDreamIds sampleSize dreamFilters userConditions = do
    E.select . E.from $ \(dream `E.InnerJoin` userAccount) -> do
        E.on (userAccount E.^. UserAccountId E.==. dream E.^. DreamUserId)
        restrictByOwnerConditions dream userConditions
        restrictByDreamFilters dream userAccount dreamFilters
        E.orderBy [E.rand]
        -- 1000 is statistically significant up to perhaps 10,000,000 rows,
        -- with a margin of error of 3% and confidence of 95%
        -- according to cursory research with tools like:
        -- https://www.surveymonkey.com/mp/sample-size-calculator/
        E.limit sampleSize 
        return (dream E.^. DreamId)


maybeNoConditions :: forall a. (a -> SqlQuery ()) -> Maybe a -> SqlQuery ()
maybeNoConditions = maybe $ pure ()

--restrictByUserConditions :: (Maybe (Key UserAccount, Bool))
restrictByOwnerConditions :: E.SqlExpr (Entity Dream) -> Maybe (Key UserAccount, Bool) -> SqlQuery ()
restrictByOwnerConditions dream userConditions =
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

restrictByDreamFilters :: E.SqlExpr (Entity Dream) -> E.SqlExpr (Entity UserAccount) -> DreamFilters -> SqlQuery ()
restrictByDreamFilters dream userAccount DreamFilters{..} = do
    -- inspired by: https://gist.github.com/bitemyapp/89c5e0663bddc3b1c78f5e3fa33e7dc4#file-companiescount-hs-L79
    maybeNoConditions (\l -> E.where_ (dream E.^. DreamIsLucid     E.==. E.val l)) filterLucid
    maybeNoConditions (\n -> E.where_ (dream E.^. DreamIsNightmare E.==. E.val n)) filterNightmare
    maybeNoConditions (\r -> E.where_ (dream E.^. DreamIsRecurring E.==. E.val r)) filterRecurring
    maybeNoConditions (\es -> E.where_ (E.just (dream E.^. DreamEmotions) E.@>. (E.jsonbVal es))) filterEmotions
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


-- | Ad-hoc queries

data KeywordStatsDB = KeywordStatsDB {
    keyword :: Text,
    lucidCount :: Int,
    nightmareCount :: Int,
    recurringCount :: Int,
    totalDreams :: Int,
    topEmotion :: EmotionLabel
} deriving (Eq, Show)

data EmotionStatsDB = EmotionStatsDB {
    emotionName :: EmotionLabel,
    eLucidCount :: Int,
    eNightmareCount :: Int,
    eRecurringCount :: Int,
    eTotalDreams :: Int
} deriving (Eq, Show)

keywordStats :: Range -> Int -> QueryM [KeywordStatsDB]
keywordStats range n = do
    topWords <- commonWordStats range n
    withEmotions <- 
        forM topWords $ \((w, l, ni, r, t)) -> do
            perhapsEmotion <- topEmotionForKeyword range w
            if (not $ null perhapsEmotion) then
                pure $ Just $ (w,l,ni,r,t,(perhapsEmotion & head & fst))
            else
                pure Nothing
    return $ map (\(w,l,ni,r,t,te)-> KeywordStatsDB w l ni r t (EmotionLabel te)) 
                 (catMaybes withEmotions)

emotionStats :: Range -> Int -> QueryM [EmotionStatsDB]
emotionStats range n =
    (mostCommonEmotions range n) >>= mapM mkEmotionStats
    where
        mkEmotionStats (e, l, ni, r, t) = pure $ EmotionStatsDB (EmotionLabel e) l ni r t

{-
SCARY RAW SQL ZONE

TYPE SAFETY IS FRAGILE HERE

TREAD LIGHTLY!
-}

-- | Get tuples of word => occurences, up to N most common words
-- see notes and test scripts in https://gist.github.com/lfborjas/2fd2d237d5600b392231ae2c472017bb
-- for easy REPL testing, with `stack ghci`, e.g.
-- *> w <- runDBSimple "postgresql://localhost/undercurrent_dev?user=luis" $ mostCommonWords 3
-- yields
-- [("recurring",1728),("prophecy",1728),("smol",1728)]
commonWordStats :: Range -> Int -> QueryM [(Text, Int, Int, Int, Int)]
commonWordStats (Range start end) n = [sqlQQ|
    select c.word, 
    count (dream.id) filter (where is_lucid = true) as are_lucid,
        count (dream.id) filter (where is_nightmare = true) as are_nightmare,
        count (dream.id) filter (where is_recurring = true) as are_recurring,
        count (*) as total_dreams 
    from dream join
        (select word, ndoc from ts_stat($$select to_tsvector('english_simple', title || ' ' || description) from dream
        where dreamed_at between #{start} and #{end}$$)
        order by ndoc desc limit #{n}) as c
        on to_tsvector ('english_simple', title || ' ' || description) @@ to_tsquery('english_simple', c.word)
    where dreamed_at between #{start} and #{end}
    group by c.word
    order by total_dreams desc
    limit #{n};
|] & (fmap . map) asStats
    where
        asStats ((Single word), 
                (Single lucid), 
                (Single nightmare), 
                (Single recurring), 
                (Single total)) =
                    (word, lucid, nightmare, recurring, total)

topEmotionForKeyword :: Range -> Text -> QueryM [(Text, Int)]
topEmotionForKeyword (Range start end) w = [sqlQQ|
    select emotion #>> '{}', count (*) as c from dream cross join lateral jsonb_array_elements (emotions::jsonb) as emotion
    where to_tsvector ('english_simple', title || ' ' || description) @@ to_tsquery ('english_simple', #{w})
    and dreamed_at between #{start} and #{end}
    group by emotion order by c desc limit 1
|] & (fmap . map) (\(word, count) -> (unSingle word :: Text, unSingle count :: Int))

-- `emotion` is returned as a json value, and as such, it's quoted in the DB.
-- the `#>>` operator will extract it as actual text:
-- https://www.postgresql.org/docs/current/functions-json.html#FUNCTIONS-JSON-OP-TABLE
mostCommonEmotions :: Range -> Int -> QueryM [(Text, Int, Int, Int, Int)]
mostCommonEmotions (Range start end) n = [sqlQQ|
    select emotion #>> '{}', 
        count (dream.id) filter (where is_lucid = true) as are_lucid,
        count (dream.id) filter (where is_nightmare = true) as are_nightmare,
        count (dream.id) filter (where is_recurring = true) as are_recurring,
        count (*) as total_dreams
    from dream cross join lateral jsonb_array_elements (emotions::jsonb) as emotion
    where dreamed_at between #{start} and #{end}
    group by emotion order by total_dreams desc limit #{n}
|] & (fmap . map) (\(a,b,c,d,e) -> 
    (unSingle a :: Text, unSingle b :: Int, unSingle c :: Int, unSingle d :: Int, unSingle e :: Int))

{-
Some more notes at:
https://gist.github.com/lfborjas/2fd2d237d5600b392231ae2c472017bb

an interesting reject: 

 select word, e, count from 
 (select distinct on (c.word) c.word, emotion #>> '{}' as e, count(*)        
  from dream cross join lateral jsonb_array_elements (emotions::jsonb) as emotion                       
  join (select word, ndoc from ts_stat($$select to_tsvector('english_simple', title || ' ' || description) from dream$$)) as c                        
  on to_tsvector ('english_simple', title || ' ' || description) @@ to_tsquery('english_simple', c.word)                   
  and dreamed_at between '2020-01-01' and '2020-07-02'                                                                      
  group by c.word, e                                                                                                      
  order by c.word, count(*) desc                                                                                           
 ) t 
 order by count desc limit 10;                                                                                                                     

-}

-- | Ugly nuclear DB function only good for testing! 
dropModels :: (MonadIO m) => SqlPersistT m ()
dropModels = rawExecute
    "TRUNCATE TABLE user_account, dream RESTART IDENTITY"
    []
