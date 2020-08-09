{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes#-}

module HandlersSpec (spec) where

import Import
import Test.Hspec
import Test.Hspec.Wai.JSON
import ApiTypes
import Handlers
import Data.Aeson (decode)

spec :: Spec
spec = do
    describe "parseLocation" $ do
        it "can parse a Suggestion from Algolia" $ do
            let apiLocation = [json|{
                "name": "Manhattan",
                "administrative": "New York",
                "county": "New York County",
                "country": "United States of America",
                "countryCode": "us",
                "type": "city",
                "latlng": {
                  "lat": 40.7834,
                  "lng": -73.9663
                },
                "highlight": {
                  "name": "<em>Manhattan</em>",
                  "administrative": "New York",
                  "country": "United States of America",
                  "county": "<em>Manhattan</em> (New York County)"
                },
                "value": "Manhattan, New York, United States of America"
            }|] & decode :: Maybe APILocation
                loc = Location {
                    city = Just "Manhattan",
                    region = Just "New York",
                    country = Just "United States of America",
                    latitude = Just 40.7834,
                    longitude = Just $ -73.9663
                }
            parseLocation <$> apiLocation `shouldBe` Just loc
