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
import Models (runMigrations, dropModels)
import Database.Persist.Postgresql (ConnectionPool, runSqlPool)
import Servant.Server (Context(..))
import Network.HTTP.Types (methodPost)

testDB :: DatabaseUrl
testDB = "postgresql://localhost/undercurrent_test?user=luis"
noOpLog :: LogFunc
noOpLog = mkLogFunc $ (\_ _ _ _ -> pure ())

testApp ::  IO Application
testApp = do
    jwtKey <- generateKey
    pool <- makeDBConnectionPool testDB
    runSqlPool runMigrations pool
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

postJSON p = request methodPost p [("Content-Type", "application/json")]

spec :: Spec
spec = 
    with testApp $ do
        describe "GET /docs" $ do
            it "returns a plain text file with docs" $ do
                get "/docs" `shouldRespondWith` 200

        describe "POST /api/login" $ do
            it "responds with 401 for an unknown user" $ do
                postJSON "/api/login" [json|{email: "rando@rando.net", password: "hunter2"}|]
                    `shouldRespondWith` 401