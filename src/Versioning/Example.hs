{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Versioning.Example where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LazyBS
import           GHC.Generics         (Generic)

import           Versioning.Base
import           Versioning.JSON
import           Versioning.Upgrade

data Foo v = Foo
    { always  :: Int               -- this field exists in all versions
    , sinceV2 :: Since V2 v Bool   -- this field has been introduced in V2
    , sinceV3 :: Since V3 v String -- this field has been introduced in V3
    , untilV2 :: Until V2 v Double -- this field has been removed in V3
    } deriving (Generic)

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
foo1 = Foo {always = 1, sinceV2 = na, sinceV3 = na, untilV2 = 3.14}

-- | Upgrade a 'Foo' from V1 to V3
upgradeFoo :: Foo V1 -> Foo V3
upgradeFoo = upgrade

-- | Decode a 'Foo' of whatever version and upgrade it to V3
decodeFoo :: LazyBS.ByteString -> Maybe (Foo V3)
decodeFoo = decodeAnyVersion
