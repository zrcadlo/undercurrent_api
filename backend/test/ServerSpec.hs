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
import ApiTypes (AuthenticatedUser(..), UserId(..))
import Server (app)
import Run (makeDBConnectionPool)
import Models (Dream(..), UserAccount(..), dropModels)
import Database.Persist.Postgresql (insertMany, insert, withPostgresqlConn, runSqlConn)
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
import Helpers
import qualified Database.Esqueleto.PostgreSQL.JSON as E

noOpLog :: LogFunc
noOpLog = mkLogFunc $ (\_ _ _ _ -> pure ())

jwtCfg :: JWTSettings
jwtCfg =  defaultJWTSettings $ unsafePerformIO $ generateKey
{-# NOINLINE jwtCfg #-}


testApp ::  IO Application
testApp = do
    let dbUrl = "postgresql://localhost/undercurrent_test?user=luis"
    pool <- makeDBConnectionPool dbUrl
    let 
        cookieCfg = defaultCookieSettings 
        cfg = cookieCfg :. jwtCfg :. EmptyContext
        ctx = App
            { appLogFunc = noOpLog
            , appPort = 3033
            , appDatabaseUrl = dbUrl
            , appDBPool = pool
            }
        in 
            return $ app cfg cookieCfg jwtCfg ctx

mkJSONLocation :: Text  -> Text -> (Maybe (E.JSONB Location))
mkJSONLocation c co = Just $ E.JSONB $ Location {city = Just c, region = Nothing, country = Just co, latitude = Nothing, longitude = Nothing}

setupData :: IO ()
setupData = runNoLoggingT $ withPostgresqlConn testDB . runSqlConn $ do
    dropModels
    
    -- insert some "givens"
    hashedPw <- hashPassword "secureAlpacaPassword"
    nena <- insert $
         UserAccount "nena@alpaca.net"
            hashedPw
            "Nena Alpaca"
            (Just Female)
            (Just (UTCTime (fromGregorian 2017 2 14) 0))
            (mkJSONLocation "Tokyo" "Japan")
            (Just Scorpio)
            zeroTime
            zeroTime

    charlie <- insert $
            UserAccount "charlie@alpaca.net"
                hashedPw
                "Charlie Alpaca"
                (Just Male)
                (Just zeroTime)
                (mkJSONLocation "Queens" "US")
                (Just Cancer)
                zeroTime
                zeroTime

    paco <- insert $
            UserAccount "paco@alpaca.net"
                hashedPw
                "Paco Alpaca"
                (Just NonBinary)
                (Just zeroTime)
                (mkJSONLocation "Tokyo" "Japan")
                (Just Capricorn)
                zeroTime
                zeroTime

    _ <- insertMany $ 
        -- TODO: actually situate in some location!
        [(Dream nena "Nena's dream" "Nena dreams" False False True False False (JSONB [EmotionLabel "joy"]) (UTCTime (fromGregorian 2017 2 14) 0) zeroTime zeroTime Nothing)
        ,(Dream nena "Nena's secret dream" "Nena dreams" False False True True False (JSONB [EmotionLabel "joy"]) (UTCTime (fromGregorian 2017 2 14) 0) zeroTime zeroTime Nothing)
        ]

    _ <- insertMany $
        [(Dream charlie "Charlies's dream" "Charlie dreams" False False True False False (JSONB [EmotionLabel "joy", EmotionLabel "surprise"]) (UTCTime (fromGregorian 2017 2 14) 0) zeroTime zeroTime Nothing)
        ,(Dream charlie "Charlie's secret dream" "Charlie dreams" False False True True False (JSONB [EmotionLabel "joy"]) (UTCTime (fromGregorian 2017 2 14) 0) zeroTime zeroTime Nothing)
        ]

    _ <- insertMany $
        [(Dream paco "Paco's secret dream" "Paco dreams" False False True True False (JSONB [EmotionLabel "joy"]) (UTCTime (fromGregorian 2017 2 14) 0) zeroTime zeroTime Nothing)]

    return ()

-- this is horrible, I'm sorry Hubert:
-- in `dropModels`, we explicitly reset the sequence so the first user (Nena) is always #1
testUserId :: Int64
testUserId = 1

spec :: Spec
spec = 
    -- before the entire test suite, migrate the schema, drop any existing data, and populate with "seed" data
    beforeAll_ (migrate >> setupData) $ with testApp $ do
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
                        username: "Nena Alpaca",
                        gender: "Female",
                        birthday: "2017-02-14T00:00:00Z",
                        location: {city: "Tokyo", country: "Japan", region: null, latitude: null, longitude: null},
                        zodiac_sign: "Scorpio"
                    }|] {matchStatus = 200}

        describe "PUT /api/user" $ do
            it "responds successfully when updating email and name" $ do
                authenticatedPut "/api/user" currentUserToken
                    [json|{username: "Another Nena", location: {name: "Shenzhen", country: "China", type: "city"}, zodiac_sign: "Cancer"}|]
                    `shouldRespondWith` 204

        describe "PUT /api/user/password" $ do
            it "responds with 403 if given the wrong password" $ do
                authenticatedPut "/api/user/password" currentUserToken
                    [json|{current_password: "notTheRightOne", new_password: "aNewOne"}|]
                    `shouldRespondWith` 403
            
            it "responds with 204 if given the right password" $ do
                authenticatedPut "/api/user/password" currentUserToken
                    [json|{current_password: "secureAlpacaPassword", new_password: "aNewPassword"}|]
                    `shouldRespondWith` 204

        describe "POST /api/users" $ do
            it "responds with 400 if required data is missing" $ do
                postJSON "/api/users" [json|{username: "Luis"}|]
                    `shouldRespondWith` 400

            it "responds with 409 if trying to create a user with an existing email" $ do
                postJSON "/api/users" 
                    [json|{
                        "email":"nena@alpaca.net",
                        "birthday":"2017-02-14T00:00:00Z",
                        "gender":"Male",
                        "username":"Paco Alpaco",
                        "password":"somePassword",
                        "location": {name: "Shenzhen", country: "China", type: "city"}
                    }|]
                    `shouldRespondWith` 409

            it "responds with 409 if trying to create a user with an existing email (case insensitive)" $ do
                postJSON "/api/users" 
                    [json|{
                        "email":"NENA@Alpaca.Net",
                        "birthday":"2017-02-14T00:00:00Z",
                        "gender":"Male",
                        "username":"Paco Alpaco",
                        "password":"somePassword",
                        "location": {name: "Shenzhen", country: "China", type: "city"}
                    }|]
                    `shouldRespondWith` 409                    

            it "responds with 409 if trying to create a user with an existing username (case insensitive)" $ do
                postJSON "/api/users" 
                    [json|{
                        "email":"new@alpaca.Net",
                        "birthday":"2017-02-14T00:00:00Z",
                        "gender":"Male",
                        "username":"CHARLIE ALPACA",
                        "password":"somePassword",
                        "location": {name: "Shenzhen", country: "China", type: "city"}
                    }|]
                    `shouldRespondWith` 409                    
            
            it "responds with 201 when creating a new user" $ do
                postJSON "/api/users" 
                    [json|{
                        "email":"paco.new@alpaca.net",
                        "birthday":"2017-02-14T00:00:00Z",
                        "gender":"Male",
                        "username":"Paco Alpaco",
                        "password":"somePassword",
                        "location": {name: "Shenzhen", country: "China", type: "city"}
                    }|]
                    `shouldRespondWith` 201

        describe "GET /api/dreams?mine" $ do
            it "responds with 401 if no user is signed in" $ do
                get "/api/dreams?mine" `shouldRespondWith` 401
            
            it "responds with the current user's dreams, private and public, when authenticated" $ do
                authenticatedGet "/api/dreams?mine" currentUserToken ""
                    `shouldRespondWith` [json|[
                        {"nightmare":false,
                        "lucid":false,
                        "private":true,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dream_id":2,
                        "title":"Nena's secret dream",
                        "description":"Nena dreams",
                        "dreamer_username": "Another Nena",
                        "dreamer_location": "Shenzhen, China",
                        "dreamer_zodiac_sign": "Cancer",
                        "dreamer_gender": "Female"},
                        {"nightmare":false,
                        "lucid":false,
                        "private":false,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dream_id":1,
                        "title":"Nena's dream",
                        "description":"Nena dreams",
                        "dreamer_username": "Another Nena",
                        "dreamer_location": "Shenzhen, China",
                        "dreamer_zodiac_sign": "Cancer",
                        "dreamer_gender": "Female"
                        }]|] {matchStatus = 200}

            it "can filter your dreams, too" $ do
                let onlyTheSecretDream = [json|[
                     {"nightmare":false,
                        "lucid":false,
                        "private":true,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dream_id":2,
                        "title":"Nena's secret dream",
                        "description":"Nena dreams",
                        "dreamer_username": "Another Nena",
                        "dreamer_location": "Shenzhen, China",
                        "dreamer_zodiac_sign": "Cancer",
                        "dreamer_gender": "Female"}
                ]|]
                authenticatedGet "/api/dreams?mine&keywords=secret" currentUserToken ""
                    `shouldRespondWith` onlyTheSecretDream {matchStatus = 200 }

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
                        "emotions":["joy", "intimidated", "worry"],
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
                    [json|{nightmare: true, emotions: ["vigilant", "worry"], private:false}|]
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

        describe "GET /api/dreams?username=Another%20Nena" $ do
            it "responds with all dreams, public and private, if the current user is the owner" $ do
                authenticatedGet "/api/dreams?username=Another%20Nena" currentUserToken ""
                    `shouldRespondWith` [json|[
                        {"nightmare":false,
                        "lucid":false,
                        "private":true,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dream_id":2,
                        "title":"Nena's secret dream",
                        "description":"Nena dreams",
                        "dreamer_username": "Another Nena",
                        "dreamer_location": "Shenzhen, China",
                        "dreamer_zodiac_sign": "Cancer",
                        "dreamer_gender": "Female"},
                        {"nightmare":false,
                        "lucid":false,
                        "private":false,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dream_id":1,
                        "title":"Nena's dream",
                        "description":"Nena dreams",
                        "dreamer_username": "Another Nena",
                        "dreamer_location": "Shenzhen, China",
                        "dreamer_zodiac_sign": "Cancer",
                        "dreamer_gender": "Female"
                        }]|] {matchStatus = 200}

            it "responds with public dreams when the current user is someone else" $ do
                authenticatedGet "/api/dreams?username=Another%20Nena" charlieUserToken ""
                    `shouldRespondWith` [json|[
                        {"nightmare":false,
                        "lucid":false,
                        "private":false,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dream_id":1,
                        "title":"Nena's dream",
                        "description":"Nena dreams",
                        "dreamer_username": "Another Nena",
                        "dreamer_location": "Shenzhen, China",
                        "dreamer_zodiac_sign": "Cancer",
                        "dreamer_gender": "Female"
                        }]|] {matchStatus = 200}

            it "responds with public dreams when there's no current user" $ do
                get "/api/dreams?username=Another%20Nena"
                    `shouldRespondWith` [json|[
                        {"nightmare":false,
                        "lucid":false,
                        "private":false,
                        "emotions":["joy"],
                        "recurring":true,
                        "date":"2017-02-14T00:00:00Z",
                        "starred":false,
                        "dream_id":1,
                        "title":"Nena's dream",
                        "description":"Nena dreams",
                        "dreamer_username": "Another Nena",
                        "dreamer_location": "Shenzhen, China",
                        "dreamer_zodiac_sign": "Cancer",
                        "dreamer_gender": "Female"
                        }]|] {matchStatus = 200}

            it "can filter dreams as well" $ do
                get "/api/dreams?username=Another%20Nena&lucid=true"
                    `shouldRespondWith` [json|[]|] {matchStatus = 200 } 

        describe "GET /api/dreams" $ do
            context "all recent dreams" $ do
                it "finds the most recent dreams" $ do
                    let recentDreams = [json|[
                        {"nightmare":false,"lucid":false,"dreamer_location":"Tokyo, Japan","private":false,"emotions":["joy","intimidated","worry"],"recurring":true,"dreamer_zodiac_sign":"Capricorn","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":6,"dreamer_gender":"NonBinary","title":"I dream of Alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"Paco Alpaca"},{"nightmare":true,"lucid":false,"dreamer_location":"Queens","private":false,"emotions":["vigilant","worry"],"recurring":true,"dreamer_zodiac_sign":"Cancer","date":"2017-02-14T00:00:00Z","starred":false,"dream_id":4,"dreamer_gender":"Male","title":"Charlie's secret dream","description":"Charlie dreams","dreamer_username":"Charlie Alpaca"}                        
                    ]|]
                    get "/api/dreams?limit=2"
                        `shouldRespondWith` recentDreams {matchStatus = 200}

            context "with user filters" $ do
                it "finds public dreams that match the user filters" $ do
                    let pacoDreams = [json|[
                        {"nightmare":false,"lucid":false,"dreamer_location":"Tokyo, Japan","private":false,"emotions":["joy","intimidated","worry"],"recurring":true,"dreamer_zodiac_sign":"Capricorn","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":6,"dreamer_gender":"NonBinary","title":"I dream of Alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"Paco Alpaca"}
                    ]|]
                    get "/api/dreams?location=Tokyo,%20Japan"
                        `shouldRespondWith` pacoDreams {matchStatus = 200}
                    get "/api/dreams?gender=nonBinary"
                        `shouldRespondWith` pacoDreams {matchStatus = 200}
                    get "/api/dreams?zodiac_sign=capricorn"
                        `shouldRespondWith` pacoDreams {matchStatus = 200}
                    get "/api/dreams?gender=nonBinary&location=Tokyo,%20Japan&zodiac_sign=capricorn"
                        `shouldRespondWith` pacoDreams {matchStatus = 200}
            context "with dream flags" $ do
                it "finds public dreams that match the given dream flags" $ do
                    let charlieDreams = [json|[
                        {"nightmare":true,"lucid":false,"dreamer_location":"Queens","private":false,"emotions":["vigilant","worry"],"recurring":true,"dreamer_zodiac_sign":"Cancer","date":"2017-02-14T00:00:00Z","starred":false,"dream_id":4,"dreamer_gender":"Male","title":"Charlie's secret dream","description":"Charlie dreams","dreamer_username":"Charlie Alpaca"}
                    ]|]
                    get "/api/dreams?nightmare=true"
                        `shouldRespondWith` charlieDreams {matchStatus = 200}
                    get "/api/dreams?nightmare=true&lucid=false&recurring=true"
                        `shouldRespondWith` charlieDreams {matchStatus = 200}
            context "with complex dream filters" $ do
                it "finds dreams with the given emotions" $ do
                    let sadPacoDreams = [json|[
                        {"nightmare":false,"lucid":false,"dreamer_location":"Tokyo, Japan","private":false,"emotions":["joy","intimidated","worry"],"recurring":true,"dreamer_zodiac_sign":"Capricorn","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":6,"dreamer_gender":"NonBinary","title":"I dream of Alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"Paco Alpaca"}
                    ]|]
                    get "/api/dreams?emotions[]=intimidated&emotions[]=joy"
                        `shouldRespondWith` sadPacoDreams {matchStatus = 200}
                it "finds dreams with the given keywords" $ do
                    let weirdPacoDreams = [json|[
                        {"nightmare":false,"lucid":false,"dreamer_location":"Tokyo, Japan","private":false,"emotions":["joy","intimidated","worry"],"recurring":true,"dreamer_zodiac_sign":"Capricorn","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":6,"dreamer_gender":"NonBinary","title":"I dream of Alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"Paco Alpaca"}
                    ]|]
                    get "/api/dreams?keywords=Alpaca%20SUNGLASS"
                        `shouldRespondWith` weirdPacoDreams {matchStatus = 200}
            context "with date ranges" $ do
                it "finds dreams after a given date" $ do
                    let someDreams = [json|[
                        {"nightmare":false,"lucid":false,"dreamer_location":"Tokyo, Japan","private":false,"emotions":["joy","intimidated","worry"],"recurring":true,"dreamer_zodiac_sign":"Capricorn","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":6,"dreamer_gender":"NonBinary","title":"I dream of Alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"Paco Alpaca"}
                    ]|]
                    get "/api/dreams?after=2020-07-07T00:00:00Z"
                        `shouldRespondWith` someDreams {matchStatus = 200}
                it "finds dreams before a given date" $ do
                    let someMoreDreams = [json|[
                        {"nightmare":false,"lucid":false,"dreamer_location":"Tokyo, Japan","private":false,"emotions":["joy","intimidated","worry"],"recurring":true,"dreamer_zodiac_sign":"Capricorn","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":6,"dreamer_gender":"NonBinary","title":"I dream of Alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"Paco Alpaca"},{"nightmare":true,"lucid":false,"dreamer_location":"Queens","private":false,"emotions":["vigilant","worry"],"recurring":true,"dreamer_zodiac_sign":"Cancer","date":"2017-02-14T00:00:00Z","starred":false,"dream_id":4,"dreamer_gender":"Male","title":"Charlie's secret dream","description":"Charlie dreams","dreamer_username":"Charlie Alpaca"}
                    ]|]
                    get "/api/dreams?before=2020-07-07T00:00:00Z&limit=2"
                        `shouldRespondWith` someMoreDreams {matchStatus=200}
                it "finds dreams in a given range" $ do
                    let thoseSameDreams = [json|[
                        {"nightmare":false,"lucid":false,"dreamer_location":"Tokyo, Japan","private":false,"emotions":["joy","intimidated","worry"],"recurring":true,"dreamer_zodiac_sign":"Capricorn","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":6,"dreamer_gender":"NonBinary","title":"I dream of Alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"Paco Alpaca"}
                    ]|]
                    get "/api/dreams?before=2020-08-01T00:00:00Z&after=2020-07-07T00:00:00Z"
                        `shouldRespondWith` thoseSameDreams {matchStatus=200}
            context "with pagination parameters" $ do
                it "finds a second page of dreams" $ do
                    let nextDreams = [json|[
                        {"nightmare":true,"lucid":false,"dreamer_location":"Queens","private":false,"emotions":["vigilant","worry"],"recurring":true,"dreamer_zodiac_sign":"Cancer","date":"2017-02-14T00:00:00Z","starred":false,"dream_id":4,"dreamer_gender":"Male","title":"Charlie's secret dream","description":"Charlie dreams","dreamer_username":"Charlie Alpaca"}
                    ]|]
                    -- without last_seen_id=6, we'd also get Paco's dream; without limit=1,
                    -- we should see two of Charlie's dreams.
                    get "/api/dreams?before=2020-07-07T00:00:00Z&limit=1&last_seen_id=6"
                        `shouldRespondWith` nextDreams {matchStatus=200}

            context "with bad filters" $ do
                it "gets a bad request error" $ do
                    get "/api/dreams?before=yesterday"
                        `shouldRespondWith` "Error parsing query parameter before failed: Failed reading: date must be of form [+,-]YYYY-MM-DD" {matchStatus = 400}

                    -- an unrecognized parameter will simply be ignored
                    get "/api/dreams?zodiacSign=libra&limit=1"
                        `shouldRespondWith` 200
                    
                    get "/api/dreams?zodiac_sign=liberace"
                        `shouldRespondWith` "Error parsing query parameter zodiac_sign failed: could not parse: `liberace'" {matchStatus = 400}

                    get "/api/dreams?gender=bender"
                        `shouldRespondWith` "Error parsing query parameter gender failed: could not parse: `bender'" {matchStatus = 400}

                    get "/api/dreams?username=notexist"
                        `shouldRespondWith` 404

                    get "/api/dreams?lucid=nope"
                        `shouldRespondWith` "Error parsing query parameter lucid failed: could not parse: `nope'" {matchStatus = 400}

                    get "/api/dreams?emotions=joy&emotions=joy2"
                        `shouldRespondWith` "Error parsing query parameter(s) emotions failed: joy2 is not an emotion known to our database!" {matchStatus = 400}

        describe "GET /api/stats" $ do
            -- TODO: more contexts!
            context "public stats" $ do
                it "gets top keywords and emotions for a sample of all dreams" $ do
                    let statsForAll = [json|
                        {
                          "top_emotions":[{"recurring_count":3,"nightmare_count":0,"lucid_count":0,"total_dreams":3,"name":"joy"},{"recurring_count":2,"nightmare_count":1,"lucid_count":0,"total_dreams":2,"name":"worry"},{"recurring_count":1,"nightmare_count":1,"lucid_count":0,"total_dreams":1,"name":"vigilant"},{"recurring_count":1,"nightmare_count":0,"lucid_count":0,"total_dreams":1,"name":"intimidated"},{"recurring_count":1,"nightmare_count":0,"lucid_count":0,"total_dreams":1,"name":"surprise"}],
                          "top_keywords":[{"recurring_count":4,"nightmare_count":1,"lucid_count":0,"total_dreams":4,"top_emotion":"joy","keyword":"dream"},{"recurring_count":3,"nightmare_count":1,"lucid_count":0,"total_dreams":3,"top_emotion":"joy","keyword":"dreams"},{"recurring_count":2,"nightmare_count":1,"lucid_count":0,"total_dreams":2,"top_emotion":"joy","keyword":"charlie"},{"recurring_count":1,"nightmare_count":1,"lucid_count":0,"total_dreams":1,"top_emotion":"worry","keyword":"secret"},{"recurring_count":1,"nightmare_count":0,"lucid_count":0,"total_dreams":1,"top_emotion":"joy","keyword":"sunglasses"},{"recurring_count":1,"nightmare_count":0,"lucid_count":0,"total_dreams":1,"top_emotion":"joy","keyword":"alpacas"},{"recurring_count":1,"nightmare_count":0,"lucid_count":0,"total_dreams":1,"top_emotion":"joy","keyword":"wearing"},{"recurring_count":1,"nightmare_count":0,"lucid_count":0,"total_dreams":1,"top_emotion":"joy","keyword":"charlies"},{"recurring_count":1,"nightmare_count":0,"lucid_count":0,"total_dreams":1,"top_emotion":"joy","keyword":"nena"}]
                        }
                    |]
                    get "/api/stats"
                        `shouldRespondWith` statsForAll {matchStatus = 200}
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
