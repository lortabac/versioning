{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Versioning.JSON
  ( DecodeAnyVersion
  , decodeAnyVersion
  )
where

import           Control.Applicative  ((<|>))
import           Data.Aeson           (FromJSON, decode)
import qualified Data.ByteString.Lazy as LazyBS
import           Data.Kind            (Type)
import           GHC.TypeLits

import           Versioning.Base
import           Versioning.Upgrade

-- | Decode a JSON string by trying all the versions decrementally
--   and upgrade the decoded object to the newest version.
decodeAnyVersion
  :: forall v a . DecodeAnyVersion v v a => LazyBS.ByteString -> Maybe (a v)
decodeAnyVersion = decodeAnyVersion' @v @v

class DecodeAnyVersion (v :: V) (w :: V) (a :: V -> Type) where
    decodeAnyVersion' :: LazyBS.ByteString -> Maybe (a w)

instance {-# OVERLAPPING #-} (FromJSON (a V1), Upgrade V1 w a) => DecodeAnyVersion V1 w a where
    decodeAnyVersion' bs = upgrade @V1 @w <$> decode @(a V1) bs

instance {-# OVERLAPPABLE #-} (DecodeAnyVersion (Decr v V1) w a, FromJSON (a v), FromJSON (a (Decr v V1)), Upgrade v w a)
  => DecodeAnyVersion v w a where
    decodeAnyVersion' bs = upgrade @v @w <$> decode @(a v) bs
                       <|> decodeAnyVersion' @(Decr v V1) @w bs

-- | Decrement version until the target version is reached
type family Decr (c :: V) (v :: V) :: V where
    Decr ('V c) ('V v) = Decr' (CmpNat c v) ('V c)

type family Decr' (o :: Ordering) (c :: V) :: V where
    Decr' 'GT ('V c) = 'V (c - 1)
    Decr' 'EQ ('V c) = 'V c
    Decr' 'LT ('V c) = TypeError
                       ( 'Text "Cannot decrement "
                   ':<>: 'ShowType ('V c)
                   ':<>: 'Text " because the target is a higher version"
                       )
