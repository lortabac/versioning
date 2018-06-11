{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Versioning.JSON
  ( Applied
  , DecodeAnyVersion
  , WithAnyVersion
  , decodeAnyVersion
  , withAnyVersion
  , withAnyVersionM
  )
where

import           Control.Applicative   ((<|>))
import           Data.Aeson            (FromJSON, decode)
import qualified Data.ByteString.Lazy  as LazyBS
import           Data.Functor.Identity (Identity (..))
import           Data.Kind             (Constraint, Type)
import           GHC.TypeLits

import           Versioning.Base
import           Versioning.Upgrade

-- | Decode a JSON string by trying all the versions decrementally
--   and upgrade the decoded object to the newest version.
decodeAnyVersion
  :: forall v a . DecodeAnyVersion v v a => LazyBS.ByteString -> Maybe (a v)
decodeAnyVersion = decodeAnyVersion' @v @v

-- | Decode a JSON string by trying all the versions decrementally
--   and apply an action to the decoded object at its original version.
withAnyVersionM
  :: forall c v a m
   . (WithAnyVersion v a c, Applicative m, c a v)
  => ApplyM m a c
  -> LazyBS.ByteString
  -> m (Maybe (Applied c a))
withAnyVersionM = withAnyVersion' @v @a @c

-- | Pure version of 'withAnyVersionM'.
withAnyVersion
  :: forall c v a
   . (WithAnyVersion v a c, c a v)
  => Apply a c
  -> LazyBS.ByteString
  -> Maybe (Applied c a)
withAnyVersion action = runIdentity . withAnyVersionM @c @v @a (Identity . action)

-- | The result type of the action that has been applied to the decoded object
--   with 'withAnyVersion'.
type family Applied (c :: (V -> Type) -> V -> Constraint) (a :: V -> Type) :: Type

type ApplyM m a c = forall v. c a v => a v -> m (Applied c a)

type Apply a c = forall v. c a v => a v -> Applied c a

class DecodeAnyVersion (v :: V) (w :: V) (a :: V -> Type) where
    decodeAnyVersion' :: LazyBS.ByteString -> Maybe (a w)

instance {-# OVERLAPPING #-} (FromJSON (a V1), Upgrade V1 w a) => DecodeAnyVersion V1 w a where
    decodeAnyVersion' bs = upgrade @V1 @w <$> decode @(a V1) bs

instance {-# OVERLAPPABLE #-} (DecodeAnyVersion (Decr v V1) w a, FromJSON (a v), FromJSON (a (Decr v V1)), Upgrade v w a)
  => DecodeAnyVersion v w a where
    decodeAnyVersion' bs = upgrade @v @w <$> decode @(a v) bs
                       <|> decodeAnyVersion' @(Decr v V1) @w bs

class WithAnyVersion (v :: V) (a :: V -> Type) c where
    withAnyVersion' :: (Applicative m, c a v) => ApplyM m a c -> LazyBS.ByteString -> m (Maybe (Applied c a))

instance {-# OVERLAPPING #-} (FromJSON (a V1), c a V1) => WithAnyVersion V1 a c where
    withAnyVersion' action bs = case decode @(a V1) bs of
        Just doc -> Just <$> action doc
        Nothing  -> pure Nothing

instance {-# OVERLAPPABLE #-} (WithAnyVersion (Decr v V1) a c, FromJSON (a v), FromJSON (a (Decr v V1)), c a v, c a (Decr v V1))
  => WithAnyVersion v a c where
    withAnyVersion' action bs = case decode @(a v) bs of
        Just doc -> Just <$> action doc
        Nothing  -> withAnyVersion' @(Decr v V1) @a @c action bs

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
