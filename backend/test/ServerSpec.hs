{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes#-}

module ServerSpec (spec) where

import Import
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai (Application)
import Servant.Auth.Server (makeJWT, JWTSettings, defaultJWTSettings, defaultCookieSettings, generateKey)
import Server (AuthenticatedUser(..), UserId(..), app)
import Run (makeDBConnectionPool)
import Models (Dream(..), UserAccount(..), migrateAll, dropModels)
import Database.Persist.Postgresql (insertMany, ConnectionString, runMigration, insert, withPostgresqlConn, runSqlConn)
import Servant.Server (Context(..))
import Network.HTTP.Types (methodDelete, methodGet, methodPut, methodPost)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Data.Password.Argon2 (hashPassword)
import System.IO.Unsafe (unsafePerformIO)
import Data.Either (fromRight)
import RIO.ByteString.Lazy (toStrict)
import RIO.Time (fromGregorian, UTCTime(..))
import Util
import Database.Esqueleto.PostgreSQL.JSON (JSONB(..))

testDB :: DatabaseUrl
testDB = "postgresql://localhost/undercurrent_test?user=luis"
noOpLog :: LogFunc
noOpLog = mkLogFunc $ (\_ _ _ _ -> pure ())
--testKey :: JWK
testKey = unsafePerformIO $ generateKey
jwtCfg :: JWTSettings
jwtCfg =  defaultJWTSettings testKey


testApp ::  IO Application
testApp = do
    pool <- makeDBConnectionPool testDB
    let 
        cookieCfg = defaultCookieSettings 
        cfg = cookieCfg :. jwtCfg :. EmptyContext
        ctx = App
            { appLogFunc = noOpLog
            , appPort = 3033
            , appDatabaseUrl = testDB
            , appDBPool = pool
            }
        in 
            return $ app cfg cookieCfg jwtCfg ctx

testDBBS :: ConnectionString
testDBBS = "postgresql://localhost/undercurrent_test?user=luis"

setupData :: IO ()
setupData = runNoLoggingT $ withPostgresqlConn testDBBS . runSqlConn $ do
    -- run any missing schema migrations
    _ <- runMigration migrateAll
    dropModels
    
    -- insert some "givens"
    hashedPw <- hashPassword "secureAlpacaPassword"
    nena <- insert $
         UserAccount "nena@alpaca.net"
            hashedPw
            "Nena Alpaca"
            Female
            (Just (UTCTime (fromGregorian 2017 2 14) 0))
            (Just "Tokyo, Japan") 
            (Just (UTCTime (fromGregorian 2017 2 14) 0)) 
            (Just (UTCTime (fromGregorian 2017 2 14) 0))

    charlie <- insert $
            UserAccount "charlie@alpaca.net"
                hashedPw
                "Charlie Alpaca"
                Male
                (Just zeroTime)
                (Just "Tokyo Japan")
                (Just zeroTime)
                (Just zeroTime)

    paco <- insert $
            UserAccount "paco@alpaca.net"
                hashedPw
                "Paco Alpaca"
                NonBinary
                (Just zeroTime)
                (Just "Tokyo Japan")
                (Just zeroTime)
                (Just zeroTime)

    nenaDreams <- insertMany $ 
        [(Dream nena "Nena's dream" "Nena dreams" False False True False False (JSONB [EmotionLabel "joy"]) (UTCTime (fromGregorian 2017 2 14) 0) zeroTime zeroTime)
        ,(Dream nena "Nena's secret dream" "Nena dreams" False False True True False (JSONB [EmotionLabel "joy"]) (UTCTime (fromGregorian 2017 2 14) 0) zeroTime zeroTime)
        ]

    charlieDreams <- insertMany $
        [(Dream charlie "Charlies's dream" "Charlie dreams" False False True False False (JSONB [EmotionLabel "joy", EmotionLabel "surprise"]) (UTCTime (fromGregorian 2017 2 14) 0) zeroTime zeroTime)
        ,(Dream charlie "Charlie's secret dream" "Charlie dreams" False False True True False (JSONB [EmotionLabel "joy"]) (UTCTime (fromGregorian 2017 2 14) 0) zeroTime zeroTime)
        ]

    pacoDreams <- insertMany $
        [(Dream paco "Paco's secret dream" "Paco dreams" False False True True False (JSONB [EmotionLabel "joy"]) (UTCTime (fromGregorian 2017 2 14) 0) zeroTime zeroTime)]

    return ()

-- this is horrible, I'm sorry Hubert:
-- in `dropModels`, we explicitly reset the sequence so the first user (Nena) is always #1
testUserId :: Int64
testUserId = 1

spec :: Spec
spec = 
    -- before the entire test suite, migrate the schema, drop any existing data, and populate with "seed" data
    beforeAll_ setupData $ with testApp $ do
        describe "GET /docs" $ do
            it "returns a plain text file with docs" $ do
                get "/docs" `shouldRespondWith` 200

        describe "POST /api/login" $ do
            it "responds with 401 for an unknown user" $ do
                postJSON "/api/login" [json|{email: "rando@rando.net", password: "hunter2"}|]
                    `shouldRespondWith` 401

            it "responds with 400 for a malformed request" $ do
                postJSON "/api/login" [json|{email: "bad@email.org"}|]
                    `shouldRespondWith` 400

            it "responds with 201 for a known user" $ do
                postJSON "/api/login" [json|{email: "nena@alpaca.net", password: "secureAlpacaPassword"}|]
                    `shouldRespondWith` 201

        describe "GET /api/user" $ do
            it "responds with 401 for an unknown user" $ do
                get "/api/user" `shouldRespondWith` 401

            it "responds with the user for a known user" $ do
                authenticatedGet "/api/user" currentUserToken ""
                    `shouldRespondWith` 
                    [json|{
                        email: "nena@alpaca.net",
                        name: "Nena Alpaca",
                        gender: "Female",
                        birthday: "2017-02-14T00:00:00Z",
                        birthplace: "Tokyo, Japan"
                    }|] {matchStatus = 200}

        describe "PUT /api/user" $ do
            it "responds successfully when updating email and name" $ do
                authenticatedPut "/api/user" currentUserToken
                    [json|{name: "Another Nena", birthplace: "Shenzhen, China"}|]
                    `shouldRespondWith` 204

        describe "PUT /api/user/password" $ do
            it "responds with 403 if given the wrong password" $ do
                authenticatedPut "/api/user/password" currentUserToken
                    [json|{currentPassword: "notTheRightOne", newPassword: "aNewOne"}|]
                    `shouldRespondWith` 403
            
            it "responds with 204 if given the right password" $ do
                authenticatedPut "/api/user/password" currentUserToken
                    [json|{currentPassword: "secureAlpacaPassword", newPassword: "aNewPassword"}|]
                    `shouldRespondWith` 204

        describe "POST /api/users" $ do
            it "responds with 400 if required data is missing" $ do
                postJSON "/api/users" [json|{name: "Luis"}|]
                    `shouldRespondWith` 400

            it "responds with 409 if trying to create a user with an existing email" $ do
                postJSON "/api/users" 
                    [json|{
                        "email":"nena@alpaca.net",
                        "birthday":"2017-02-14T00:00:00Z",
                        "gender":"Male",
                        "name":"Paco Alpaco",
                        "password":"somePassword",
                        "birthplace":"Shenzhen, China"
                    }|]
                    `shouldRespondWith` 409
            
            it "responds with 201 when creating a new user" $ do
                postJSON "/api/users" 
                    [json|{
                        "email":"paco.new@alpaca.net",
                        "birthday":"2017-02-14T00:00:00Z",
                        "gender":"Male",
                        "name":"Paco Alpaco",
                        "password":"somePassword",
                        "birthplace":"Shenzhen, China"
                    }|]
                    `shouldRespondWith` 201

        describe "GET /api/user/dreams" $ do
            it "responds with 401 if no user is signed in" $ do
                get "/api/user/dreams" `shouldRespondWith` 401
            
            it "responds with the current user's dreams, private and public, when authenticated" $ do
                authenticatedGet "/api/user/dreams" currentUserToken ""
                    `shouldRespondWith` [json|[
                        {"nightmare":false,
                        "lucid":false,
                        "private":true,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dreamer_id":1,
                        "dream_id":2,
                        "title":"Nena's secret dream",
                        "description":"Nena dreams"},
                        {"nightmare":false,
                        "lucid":false,
                        "private":false,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dreamer_id":1,
                        "dream_id":1,
                        "title":"Nena's dream",
                        "description":"Nena dreams"
                        }]|] {matchStatus = 200}

        describe "POST /api/user/dreams" $ do
            it "responds with 400 if unknown emotions are used" $ do
                authenticatedPost "/api/user/dreams" pacoUserToken
                    [json|{
                        "nightmare":false,
                        "lucid":false,
                        "private":false,
                        "emotions":["afeared!"],
                        "recurring":true,
                        "date":"2020-07-07T00:00:00Z",
                        "starred":true,
                        "title":"I dream of Alpacas",
                        "description":"Some alpacas were wearing sunglasses"}|]
                    `shouldRespondWith` 400

            it "responds with 201 if the dream was correctly created" $ do
                -- TODO: maybe test for the actual response too?
                authenticatedPost "/api/user/dreams" pacoUserToken
                    [json|{
                        "nightmare":false,
                        "lucid":false,
                        "private":false,
                        "emotions":["joy", "intimidated"],
                        "recurring":true,
                        "date":"2020-07-07T00:00:00Z",
                        "starred":true,
                        "title":"I dream of Alpacas",
                        "description":"Some alpacas were wearing sunglasses"}|]
                    `shouldRespondWith` 201

        describe "PUT /api/user/dreams/:dreamId" $ do
            it "responds with 401 if not authenticated" $ do
                request methodPut "/api/user/dreams/4" [("Content-Type", "application/json")]
                    [json|{nightmare: true}|]
                    `shouldRespondWith` 401
            it "responds with 403 if this is someone else's dream" $ do
                authenticatedPut "/api/user/dreams/4" pacoUserToken
                    [json|{nightmare: true}|]
                    `shouldRespondWith` 403
            it "responds with 404 if this isn't a real dream" $ do
                authenticatedPut "/api/user/dreams/666" charlieUserToken
                    [json|{recurring: true}|]
                    `shouldRespondWith` 404
            it "responds with 204 if this is your dream" $ do
                authenticatedPut "/api/user/dreams/4" charlieUserToken
                    [json|{nightmare: true, emotions: ["vigilant", "worry"]}|]
                    `shouldRespondWith` 204

        describe "DELETE /api/user/dreams/:dreamId" $ do
            it "responds with 401 if not authenticated" $ do
                request methodDelete "/api/user/dreams/4" [("Content-Type", "application/json")] ""
                    `shouldRespondWith` 401
            it "responds with 403 if this is someone else's dream" $ do
                authenticatedDelete "/api/user/dreams/5" charlieUserToken ""
                    `shouldRespondWith` 403
            it "responds with 410 if this isn't a real dream" $ do
                authenticatedDelete "/api/user/dreams/666" charlieUserToken ""
                    `shouldRespondWith` 410
            it "responds with 204 if this is your dream" $ do
                authenticatedDelete "/api/user/dreams/5" pacoUserToken ""
                    `shouldRespondWith` 204

        describe "GET /api/users/:userId/dreams" $ do
            it "responds with all dreams, public and private, if the current user is the owner" $ do
                authenticatedGet "/api/users/1/dreams" currentUserToken ""
                    `shouldRespondWith` [json|[
                        {"nightmare":false,
                        "lucid":false,
                        "private":true,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dreamer_id":1,
                        "dream_id":2,
                        "title":"Nena's secret dream",
                        "description":"Nena dreams"},
                        {"nightmare":false,
                        "lucid":false,
                        "private":false,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dreamer_id":1,
                        "dream_id":1,
                        "title":"Nena's dream",
                        "description":"Nena dreams"
                        }]|] {matchStatus = 200}

            it "responds with public dreams when the current user is someone else" $ do
                authenticatedGet "/api/users/1/dreams" charlieUserToken ""
                    `shouldRespondWith` [json|[
                        {"nightmare":false,
                        "lucid":false,
                        "private":false,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dreamer_id":1,
                        "dream_id":1,
                        "title":"Nena's dream",
                        "description":"Nena dreams"
                        }]|] {matchStatus = 200}

            it "responds with public dreams when there's no current user" $ do
                get "/api/users/1/dreams"
                    `shouldRespondWith` [json|[
                        {"nightmare":false,
                        "lucid":false,
                        "private":false,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dreamer_id":1,
                        "dream_id":1,
                        "title":"Nena's dream",
                        "description":"Nena dreams"
                        }]|] {matchStatus = 200}

        where
            makeToken :: AuthenticatedUser -> ByteString
            makeToken u = toStrict $ fromRight "bad-token" $ unsafePerformIO $ (makeJWT u jwtCfg Nothing)
            currentUserToken = makeToken $ AuthenticatedUser $ UserId testUserId
            charlieUserToken = makeToken $ AuthenticatedUser $ UserId 2
            pacoUserToken = makeToken $ AuthenticatedUser $ UserId 3
            postJSON p = request methodPost p [("Content-Type", "application/json")]
            authenticatedRequest verb p token = request verb p [("Content-Type", "application/json"), ("Authorization", "Bearer " <> token)]
            authenticatedGet = authenticatedRequest methodGet
            authenticatedPost = authenticatedRequest methodPost
            authenticatedPut  = authenticatedRequest methodPut
            authenticatedDelete = authenticatedRequest methodDelete
