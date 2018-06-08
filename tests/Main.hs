{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LazyBS
import           GHC.Generics         (Generic)
import           Test.Hspec
import           Versioning.Base
import           Versioning.JSON
import           Versioning.Upgrade

main :: IO ()
main = hspec $ do
    describe "Upgrade" $ do
        it "Can upgrade across two versions" $ do
            upgrade @V1 foo1 `shouldBe` foo3

    describe "DecodeAnyVersion" $ do
        it "Can decode from V1" $ do
            decodeAnyVersion @V3 fooJsonV1 `shouldBe` Just foo3

    describe "DecodeAnyVersion" $ do
        it "Can decode from V3" $ do
            decodeAnyVersion @V3 fooJsonV3 `shouldBe` Just foo3

data Foo v = Foo
    { always  :: Int               -- this field exists in all versions
    , sinceV2 :: Since V2 v Bool   -- this field has been introduced in V2
    , sinceV3 :: Since V3 v String -- this field has been introduced in V3
    , untilV2 :: Until V2 v Double -- this field has been removed in V3
    } deriving (Generic)

deriving instance Eq (Foo V1)

deriving instance Eq (Foo V2)

deriving instance Eq (Foo V3)

instance FromJSON (Foo V1)

instance FromJSON (Foo V2)

instance FromJSON (Foo V3)

deriving instance Show (Foo V1)

deriving instance Show (Foo V2)

deriving instance Show (Foo V3)

-- How to upcast from V1 to V2
instance Adapt V1 V2 Foo where
    adapt foo = foo { sinceV2 = True
                    , sinceV3 = na
                    , untilV2 = untilV2 foo
                    }

-- How to upcast from V2 to V3
instance Adapt V2 V3 Foo where
    adapt foo = foo { sinceV2 = sinceV2 foo
                    , sinceV3 = "hello"
                    , untilV2 = na
                    }

-- | A 'Foo' at version V1
foo1 :: Foo V1
foo1 = Foo
    { always = 1
    , sinceV2 = na
    , sinceV3 = na
    , untilV2 = 3.14
    }

-- | A 'Foo' at version V3
foo3 :: Foo V3
foo3 = Foo
    { always = 1
    , sinceV2 = True
    , sinceV3 = "hello"
    , untilV2 = Nothing
    }

fooJsonV1 :: LazyBS.ByteString
fooJsonV1 = "{\"always\":1, \"untilV2\": 3.33}"

fooJsonV3 :: LazyBS.ByteString
fooJsonV3 = "{\"always\":1, \"sinceV2\": true, \"sinceV3\": \"hello\"}"
