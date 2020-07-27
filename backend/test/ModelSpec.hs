{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ModelSpec (spec) where

import Import
import Test.Hspec
import Models
import Control.Monad.Logger (NoLoggingT(..))
import Database.Persist.Postgresql (insert_, toSqlKey, Entity(..), insert, SqlBackend)
import Data.Password.Argon2 (hashPassword)
import RIO.Time (fromGregorian, UTCTime(..))
import Util (zeroTime, mkTime)
import System.IO.Unsafe (unsafePerformIO)
import Database.Esqueleto.PostgreSQL.JSON (JSONB(..))
import Helpers

prepareDB :: ReaderT SqlBackend (NoLoggingT IO) ()
prepareDB = do
    dropModels

-- NOTE(luis) yeah, we're doing all (model) migrations, running the given spec, and then truncating all tables again
-- it's both clean and horrible at the same time.
-- Inspired by these psychos:
-- https://github.com/bitemyapp/esqueleto/blob/4dbd5339adf99e1f045c0a02211a03c79032f9cf/test/MySQL/Test.hs
run :: ReaderT SqlBackend (NoLoggingT IO) () -> IO ()
run f =  migrate >> (withDBConn $ prepareDB >> f >> dropModels)

mkUser :: Username -> Email -> Gender -> UserAccount
mkUser username email gender = 
    UserAccount
        email
        (unsafePerformIO $ hashPassword "defaultPassword") 
        username
        (Just gender) 
        (Just zeroTime)
        Nothing
        (Just Capricorn)
        zeroTime
        zeroTime

--mkDream :: Int -> 
mkDream :: Key UserAccount -> Text -> Text -> [Text] -> UTCTime -> Maybe (Bool, Bool, Bool, Bool, Bool) -> Dream
mkDream userId t d es at Nothing =
    Dream
        userId
        t
        d
        False
        False
        False
        False
        False
        (JSONB $ map EmotionLabel es)
        at
        zeroTime
        zeroTime

mkDream userId t d es at (Just (lucid, nightmare, recurring, private, starred)) =
    Dream
        userId
        t
        d
        lucid
        nightmare
        recurring
        private
        starred
        (JSONB $ map EmotionLabel es)
        at
        zeroTime
        zeroTime

dreamTitlesFor :: [(Entity Dream, Entity UserAccount)] -> [Text]
dreamTitlesFor = map (\(Entity _ d, _) -> dreamTitle d)

spec :: Spec
spec = do
    describe "filteredDreams" $ do
        it "finds dreams by user filters, returned in descending order of creation" $ do
            run $ do
                nena <- insert $ mkUser "nena@alpaca.net" "Nena" Female
                charlie <- insert $ mkUser "charlie@alpaca.net" "Charlie" NonBinary
 
                void $ forM_ ["Dream 1", "Dream 2"] $ \title -> do
                    let dream = mkDream nena title "description" ["joy"] zeroTime Nothing
                    insert_ dream
                void $ forM_ ["Dream 3", "Dream 4"] $ \title -> do
                    let privateDream = mkDream nena title "description" ["anger"] zeroTime $ Just (False, False, False, True, False)
                    insert_ privateDream
                void $ forM_ ["Dream 5"] $ \title -> do
                    insert_ $ mkDream charlie title "description" ["joy"] zeroTime Nothing

                nenaDreams <- filteredDreams noDreamFilters $ Just (nena, False)
                secretDreams <- filteredDreams noDreamFilters $ Just (nena, True)
                publicDreams <- filteredDreams noDreamFilters Nothing

                liftIO $ dreamTitlesFor nenaDreams   `shouldBe` ["Dream 2", "Dream 1"]
                liftIO $ dreamTitlesFor secretDreams `shouldBe` ["Dream 4", "Dream 3", "Dream 2", "Dream 1"]
                liftIO $ dreamTitlesFor publicDreams `shouldBe` ["Dream 5", "Dream 2" , "Dream 1"]

        it "applies the easy filters (booleans, emotions, full text)" $ do
            run $ do
                paco <- insert $ mkUser "paco@alpaca.net" "Paco" Male
                let pacoDream = mkDream paco
                insert_ $ pacoDream "Lucid" "a lucid one with cats" ["joy", "anger", "sadness"] zeroTime $ Just (True, False, False, False, False)
                insert_ $ pacoDream "Nightmare" "huy, lucidly cats spoke!" ["fear", "sadness"] zeroTime $ Just (False, True, False, False, False)
                insert_ $ pacoDream "Recurring" "again" ["anger", "confused", "joy"] zeroTime $ Just (False, False, True, False, False)
                insert_ $ pacoDream "Should never show up" "cuz it's private!" ["joy", "anger", "fear"] zeroTime $ Just (False, True, True, True, True)

                let pacoFilteredDreams fs = filteredDreams fs $ Just (paco, False)
                    pacosSecrets = noDreamFilters {filterLucid = Just False, filterRecurring = Just True, filterEmotions = Just $ map EmotionLabel ["anger"]}

                lucidDreams <- pacoFilteredDreams $ noDreamFilters {filterLucid=Just True}
                nightmares  <- pacoFilteredDreams $ noDreamFilters {filterNightmare=Just True}
                recurring   <- pacoFilteredDreams $ noDreamFilters {filterRecurring=Just True}
                gladMad     <- pacoFilteredDreams $ noDreamFilters {filterEmotions = Just $ map EmotionLabel ["joy", "anger"]}
                private     <- filteredDreams pacosSecrets $ Just (paco, True)
                lucidCat    <- pacoFilteredDreams $ noDreamFilters {filterKeyword = Just "lucid cat"}

                liftIO $ dreamTitlesFor lucidDreams `shouldBe` ["Lucid"]
                liftIO $ dreamTitlesFor nightmares `shouldBe` ["Nightmare"]
                liftIO $ dreamTitlesFor recurring `shouldBe` ["Recurring"]
                liftIO $ dreamTitlesFor gladMad `shouldBe` ["Recurring", "Lucid"]
                liftIO $ dreamTitlesFor private `shouldBe` ["Should never show up", "Recurring"]
                liftIO $ dreamTitlesFor lucidCat `shouldBe` ["Nightmare", "Lucid"]

        -- TODO: user filters (gender, zodiac sign(?), location)
        
        it "returns ranged dreams (date ranges, limits, keyset pagination)" $ do
            run $ do
                accountant <- insert $ mkUser "accountant@alpaca.net" "Accountant" NonBinary
                let accountantDream = mkDream accountant
                    mkDate m d s = UTCTime (fromGregorian m d s) 0  
                forM_ [("First", mkDate 2020 1 1), ("Second", mkDate 2020 1 2), ("Third", mkDate 2020 1 3), ("Millionth", mkDate 2020 11 11)] $ \(t, c) -> do
                    insert_ $ accountantDream t "desc" ["joy"] c Nothing

                let filteredDreams' fs = filteredDreams fs $ Just (accountant, False)
                beforeTheSecond <- filteredDreams' $ filterDate (Just $ mkDate 2020 1 2) Nothing
                afterTheSecond <- filteredDreams'  $ filterDate  Nothing (Just $ mkDate 2020 1 2)
                beginningOfJan <- filteredDreams'  $ filterDate (Just $ mkDate 2020 1 3) (Just $ mkDate 2020 1 1)
                onlyLatest <- filteredDreams' $ limitTo 1
                seenItBefore <- filteredDreams' $ (limitTo 2) {filterLastSeenId = Just $ toSqlKey 4}

                liftIO $ dreamTitlesFor beforeTheSecond `shouldBe` ["Second", "First"]
                liftIO $ dreamTitlesFor afterTheSecond `shouldBe` ["Millionth", "Third", "Second"]
                liftIO $ dreamTitlesFor beginningOfJan `shouldBe` ["Third", "Second", "First"]
                liftIO $ dreamTitlesFor onlyLatest `shouldBe` ["Millionth"]
                liftIO $ dreamTitlesFor seenItBefore `shouldBe` ["Third", "Second"]

    describe "keywordStats" $ do
        it "finds the most common words in a range" $ do
            run $ do
                dreamer <- insert $ mkUser "dreamer@alpaca.net" "Dreamer" NonBinary
                let aDream = mkDream dreamer
                forM_ [("Winter", mkTime 2020 1 1, ["sadness"]), ("Summer", mkTime 2020 7 1, ["joy"])] $ \(t, d, e) -> do
                    insert_ $ aDream t t e d Nothing

                let expectedStats = KeywordStatsDB "winter" 0 0 0 1 "sadness"
                winterStats <- keywordStats (Range (mkTime 2019 12 1) (mkTime 2020 6 1)) 10
                liftIO $ winterStats `shouldBe` [expectedStats]

    describe "emotionStats" $ do
        it "finds the most common emotions in a range" $ do
           run $ do
                dreamer <- insert $ mkUser "dreamer@alpaca.net" "Dreamer" NonBinary
                let aDream = mkDream dreamer
                forM_ [("Winter", mkTime 2020 1 1, ["sadness"]), ("Summer", mkTime 2020 7 1, ["joy"])] $ \(t, d, e) -> do
                    insert_ $ aDream t t e d Nothing

                let expectedStats = EmotionStatsDB (EmotionLabel "sadness") 0 0 0 1
                winterStats <- emotionStats (Range (mkTime 2019 12 1) (mkTime 2020 6 1)) 10
                liftIO $ winterStats `shouldBe` [expectedStats]


limitTo :: Int64 -> DreamFilters
limitTo l = noDreamFilters{filterLimit = Just l}

filterDate :: Maybe UTCTime -> Maybe UTCTime -> DreamFilters
filterDate Nothing Nothing = noDreamFilters
filterDate b Nothing = noDreamFilters {filterBefore = b}
filterDate Nothing a = noDreamFilters {filterAfter = a}
filterDate b a = noDreamFilters {filterBefore=b, filterAfter=a}
