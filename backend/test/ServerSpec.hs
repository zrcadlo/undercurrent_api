{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes#-}

module ServerSpec (spec) where

import Import
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai (Application)
import Servant.Auth.Server (defaultJWTSettings, defaultCookieSettings, generateKey)
import Server (app)
import Run (makeDBConnectionPool)
import Models (UserAccount(..), migrateAll, dropModels)
import Database.Persist.Postgresql (ConnectionString, runMigration, insert, withPostgresqlConn, runSqlConn)
import Servant.Server (Context(..))
import Network.HTTP.Types (methodPost)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Data.Password.Argon2 (hashPassword)

testDB :: DatabaseUrl
testDB = "postgresql://localhost/undercurrent_test?user=luis"
noOpLog :: LogFunc
noOpLog = mkLogFunc $ (\_ _ _ _ -> pure ())

testApp ::  IO Application
testApp = do
    jwtKey <- generateKey
    pool <- makeDBConnectionPool testDB
    let 
        jwtCfg = defaultJWTSettings jwtKey
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

--postJSON :: ByteString -> ByteString -> WaiSession st SResponse
postJSON p = request methodPost p [("Content-Type", "application/json")]

testDBBS :: ConnectionString
testDBBS = "postgresql://localhost/undercurrent_test?user=luis"

setupData :: IO ()
setupData = runNoLoggingT $ withPostgresqlConn testDBBS . runSqlConn $ do
    -- run any missing schema migrations
    _ <- runMigration migrateAll
    dropModels
    
    -- insert some "givens"
    hashedPw <- hashPassword "secureAlpacaPassword"
    _ <- insert $ UserAccount "nena@alpaca.net" hashedPw "Nena Alpaca" Female Nothing Nothing Nothing Nothing
    return ()

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
