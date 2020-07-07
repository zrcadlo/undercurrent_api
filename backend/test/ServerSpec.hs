{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes#-}

module ServerSpec (spec) where

import Import
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai (Application)
import Servant.Auth.Server (makeJWT, JWTSettings, JWT, defaultJWTSettings, defaultCookieSettings, generateKey)
import Server (AuthenticatedUser(..), UserId(..), app)
import Run (makeDBConnectionPool)
import Models (UserAccount(..), migrateAll, dropModels)
import Database.Persist.Postgresql (ConnectionString, runMigration, insert, withPostgresqlConn, runSqlConn)
import Servant.Server (Context(..))
import Network.HTTP.Types (methodGet, methodPut, methodPost)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Data.Password.Argon2 (hashPassword)
import System.IO.Unsafe (unsafePerformIO)
import Data.Either (fromRight)
import RIO.ByteString.Lazy (toStrict)
import RIO.Time (fromGregorian, UTCTime(..))

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
    _ <- insert $
         UserAccount "nena@alpaca.net"
            hashedPw
            "Nena Alpaca"
            Female
            (Just (UTCTime (fromGregorian 2017 2 14) 0))
            (Just "Tokyo, Japan") 
            (Just (UTCTime (fromGregorian 2017 2 14) 0)) 
            (Just (UTCTime (fromGregorian 2017 2 14) 0))

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
                authenticatedGet "/api/user" (makeToken (AuthenticatedUser $ UserId testUserId)) ""
                    `shouldRespondWith` 
                    [json|{
                        email: "nena@alpaca.net",
                        name: "Nena Alpaca",
                        gender: "Female",
                        birthday: "2017-02-14T00:00:00Z",
                        birthplace: "Tokyo, Japan"
                    }|] {matchStatus = 200}

        where
            makeToken :: AuthenticatedUser -> ByteString
            makeToken u = toStrict $ fromRight "bad-token" $ unsafePerformIO $ (makeJWT u jwtCfg Nothing)
            postJSON p = request methodPost p [("Content-Type", "application/json")]
            authenticatedRequest verb p token = request verb p [("Content-Type", "application/json"), ("Authorization", "Bearer " <> token)]
            authenticatedGet = authenticatedRequest methodGet
            authenticatedPost = authenticatedRequest methodPost
            authenticatedPut  = authenticatedRequest methodPut

