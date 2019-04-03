{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Tests.Versioning.Servant.Fixtures where

import           Data.Aeson
import qualified Data.ByteString.Lazy         as LazyBS
import           GHC.Generics                 (Generic)
import           Network.Wai                  (Application)
import           Servant

import           Versioning.Base
import           Versioning.JSON
import           Versioning.Servant           (VersionedJSON)
import           Versioning.Servant.Dependent
import           Versioning.Singleton
import           Versioning.Upgrade

type Api = "foo" :> ReqBody '[VersionedJSON] (Foo V2) :> Post '[JSON] (Foo V2)
      :<|> "dep" :> Capture "v" V :> Get '[EncodedJSON] (Maybe (AtSomeV Foo))

server :: Server Api
server = pure :<|> dep

dep :: V -> Maybe (AtSomeV Foo)
dep v = withSV v $ \s -> case s of
    SV0 -> Just $ AtSomeV sv (downgrade @V2 @V0 foo2)
    SV1 -> Just $ AtSomeV sv (downgrade @V2 @V1 foo2)
    SV2 -> Just $ AtSomeV sv (downgrade @V2 @V2 foo2)
    _   -> Nothing

app :: Application
app = serve (Proxy :: Proxy Api) server

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

instance ToJSON (Foo V0)

instance ToJSON (Foo V1)

instance ToJSON (Foo V2)

-- How to upcast from V0 to V1
instance Adapt V0 V1 Foo where
    adapt foo = foo { sinceV1 = True
                    , sinceV2 = na
                    , untilV1 = untilV1 foo
                    }

-- How to upcast from V1 to V2
instance Adapt V1 V2 Foo where
    adapt foo = foo { sinceV1 = sinceV1 foo
                    , sinceV2 = "hello"
                    , untilV1 = na
                    }

-- How to downcast from V1 to V0
instance Adapt V1 V0 Foo where
    adapt foo = foo { sinceV1 = na
                    , sinceV2 = na
                    , untilV1 = untilV1 foo
                    }

-- How to downcast from V2 to V1
instance Adapt V2 V1 Foo where
    adapt foo = foo { sinceV1 = sinceV1 foo
                    , sinceV2 = na
                    , untilV1 = 3.14
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
