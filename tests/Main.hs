{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy  as LazyBS
import           GHC.Generics          (Generic)
import           Test.Hspec

import           Versioning.Base
import           Versioning.JSON
import           Versioning.Upgrade

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

data Foo v = Foo
    { always  :: Int               -- this field exists in all versions
    , sinceV1 :: Since V1 v Bool   -- this field has been introduced in V1
    , sinceV2 :: Since V2 v String -- this field has been introduced in V2
    , untilV1 :: Until V1 v Double -- this field has been removed in V2
    } deriving (Generic)

deriving instance Eq (Foo V0)

deriving instance Eq (Foo V1)

deriving instance Eq (Foo V2)

instance FromJSON (Foo V0)

instance FromJSON (Foo V1)

instance FromJSON (Foo V2)

deriving instance Show (Foo V0)

deriving instance Show (Foo V1)

deriving instance Show (Foo V2)

-- How to upcast from V1 to V2
instance Adapt V0 V1 Foo where
    adapt foo = foo { sinceV1 = True
                    , sinceV2 = na
                    , untilV1 = untilV1 foo
                    }

-- How to upcast from V2 to V3
instance Adapt V1 V2 Foo where
    adapt foo = foo { sinceV1 = sinceV1 foo
                    , sinceV2 = "hello"
                    , untilV1 = na
                    }

type instance Applied Show a = String

-- | A 'Foo' at version V0
foo0 :: Foo V0
foo0 = Foo
    { always = 1
    , sinceV1 = na
    , sinceV2 = na
    , untilV1 = 3.14
    }

-- | A 'Foo' at version V2
foo2 :: Foo V2
foo2 = Foo
    { always = 1
    , sinceV1 = True
    , sinceV2 = "hello"
    , untilV1 = Nothing
    }

fooJsonV0 :: LazyBS.ByteString
fooJsonV0 = "{\"always\":1, \"untilV1\": 3.14}"

fooJsonV2 :: LazyBS.ByteString
fooJsonV2 = "{\"always\":1, \"sinceV1\": true, \"sinceV2\": \"hello\"}"
