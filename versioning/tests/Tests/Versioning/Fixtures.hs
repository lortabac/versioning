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
module Tests.Versioning.Fixtures where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LazyBS
import           GHC.Generics         (Generic)

import           Versioning.Base
import           Versioning.JSON
import           Versioning.Upgrade

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

data SumType v = A
               | B
               | MkFoo (SinceS V1 v ()) -- This constructor cannot be used before V1

sumType :: SumType V1
sumType = MkFoo ()

deriving instance Eq (SumType V1)

deriving instance Show (SumType V1)

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
