{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Data.Aeson
import           Data.Maybe                        (fromJust)
import           Network.Wai.Test                  (SResponse (simpleBody))
import           Test.Hspec
import           Test.Hspec.Wai

import           Tests.Versioning.Servant.Fixtures
import           Versioning.Base

main :: IO ()
main = hspec $ with (pure app) $ do
    let headers = [("Content-Type", "application/json")]
        decodeV2 = fromJust . decode @(Foo V2)

    describe "VersionedJSON" $ do
        it "given the latest version returns latest version" $ do
            res <- request "POST" "foo" headers fooJsonV2
            liftIO $ decodeV2 (simpleBody res) `shouldBe` decodeV2 fooJsonV2

        it "given a previous version returns latest version" $ do
            res <- request "POST" "foo" headers fooJsonV0
            liftIO $ decodeV2 (simpleBody res) `shouldBe` decodeV2 fooJsonV2
