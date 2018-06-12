{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Versioning.Internal.Base
  ( Bare
  )
where


import           Data.Aeson   (FromJSON (..), ToJSON (..), Value (..))
import           GHC.Generics (Generic)

-- | An uninhabited type.
--   We define our own type instead of using "Data.Void"
--   because we need additional instances.
--   Moreover this type is hidden to force users to use 'NA',
--   which is needed because of the 'Maybe' hack.
data Bare

deriving instance Eq Bare

deriving instance Generic Bare

deriving instance Show Bare

-- Attempting to supply a value for an absent field must produce a
-- parsing failure
instance FromJSON Bare where
    parseJSON _ = fail "An NA field should be absent or null"

-- We provide an instance to make the compiler happy.
-- In practice it will never be used.
instance ToJSON Bare where
    toJSON _ = Null
