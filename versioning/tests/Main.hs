{-# LANGUAGE TypeApplications #-}
module Main where

import           Test.Hspec

import           Versioning.Base
import           Versioning.JSON
import           Versioning.Upgrade

import           Versioning.Tests.Fixtures (Foo (..), foo0, foo2, fooJsonV0,
                                            fooJsonV2)

main :: IO ()
main = hspec $ do
    describe "Versioning" $ do
        it "Can get the version number of a record" $
            versionNumber foo0 `shouldBe` 0

    describe "Upgrade" $ do
        it "Can upgrade across two versions" $
            upgrade @V0 foo0 `shouldBe` foo2

    describe "DecodeAnyVersion" $ do
        it "Can decode from V0" $
            fromJsonAnyVersion @V2 fooJsonV0 `shouldBe` Just foo2

        it "Can decode from V2" $
            fromJsonAnyVersion @V2 fooJsonV2 `shouldBe` Just foo2

    describe "DecodeAnyVersionFrom" $ do
        it "Can decode from V1" $
            fromJsonAnyVersionFrom @V1 @V2 fooJsonV2 `shouldBe` Just foo2

        it "Should not decode V0" $
            fromJsonAnyVersionFrom @V1 @V2 fooJsonV0 `shouldBe` (Nothing :: Maybe (Foo V2))

    describe "WithAnyVersion" $ do
        -- Decode a Foo and return its string representation without upgrading it
        it "Can apply a function on the decoded object" $ do
            let Just res = withJsonAnyVersion @Show @Foo @V2 show fooJsonV0
            res `shouldBe` show foo0

    describe "WithAnyVersionFrom" $ do
        it "Can apply a function on the decoded object" $ do
            let Just res = withJsonAnyVersionFrom @V1 @Show @Foo @V2 show fooJsonV2
            res `shouldBe` show foo2

        it "Should not decode V0" $ do
            let res = withJsonAnyVersionFrom @V1 @Show @Foo @V2 show fooJsonV0
            res `shouldBe` (Nothing :: Maybe String)
