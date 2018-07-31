-- | Encoding-agnosting deserialization utilities.
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
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
module Versioning.Internal.Decoding
  ( -- * Types
    Applied
  , Apply
  , ApplyM
  , DecodableTo
  , DecodableToFrom
  , DecodeAnyVersion
  , Decoder (..)
  , WithAnyVersion
  , WithAnyVersionFrom
    -- * Decoding and upgrading
  , decodeAnyVersion
  , decodeAnyVersionFrom
    -- * Decoding and applying an action
  , withAnyVersion
  , withAnyVersionM
  , withAnyVersionFromM
  , withAnyVersionFrom
  )
where

import           Data.Functor.Alt      (Alt (..))
import           Data.Functor.Identity (Identity (..))
import           Data.Kind             (Constraint, Type)
import           Data.Type.Equality    (type (==))

import           Versioning.Base
import           Versioning.Upgrade

-- | The result type of the action that has been applied to the decoded object
--   with 'withAnyVersion' or 'withAnyVersionM'.
type family Applied (c :: Type -> Constraint) (a :: V -> Type) :: Type

-- | The pure function to apply to the decoded object with 'withAnyVersion'
type Apply a c = forall v. c (a v) => a v -> Applied c a

-- | The action to apply to the decoded object with 'withAnyVersionM'
type ApplyM m a c = forall v. c (a v) => a v -> m (Applied c a)

-- | The function that will perform the actual decoding
newtype Decoder dec enc t a = Decoder (forall v. dec (a v) => enc -> t (a v))

-- | Handy constraint synonym to be used with 'decodeAnyVersion'
type DecodableTo dec v a = (DecodeAnyVersion v v a dec, Upgrade v v a, dec (a v))

type DecodableToFrom from dec v a = (DecodeAnyVersionFrom from v v a dec, Upgrade v v a, dec (a v))

decodeAnyVersion
  :: forall v a dec enc t
   . (Alt t, Applicative t, DecodableTo dec v a)
  => Decoder dec enc t a
  -> enc
  -> t (a v)
decodeAnyVersion = decodeAnyVersionFrom @V0

-- | Decode by trying all the versions decrementally
--   and upgrade the decoded object to the newest version.
decodeAnyVersionFrom
  :: forall from v a dec enc t
   . (Alt t, Applicative t, DecodableToFrom from dec v a)
  => Decoder dec enc t a
  -> enc
  -> t (a v)
decodeAnyVersionFrom = decodeAnyVersion' @(from == v) @from @v @v

-- | Decode by trying all the versions decrementally
--   and apply an action to the decoded object at its original version.
withAnyVersionM
  :: forall v c a dec enc m t
   . (WithAnyVersion v a c dec, Alt t, Applicative t, Traversable t, Applicative m, c (a v))
  => Decoder dec enc t a
  -> ApplyM m a c
  -> enc
  -> m (t (Applied c a))
withAnyVersionM = withAnyVersionFromM @V0 @v @c

-- | Like 'withAnyVersionM', with an additional type-parameter
--   indicating the oldest version you want to be able to decode
withAnyVersionFromM
  :: forall from v c a dec enc m t
   . (WithAnyVersionFrom from v a c dec, Alt t, Applicative t, Traversable t, Applicative m, c (a v))
  => Decoder dec enc t a
  -> ApplyM m a c
  -> enc
  -> m (t (Applied c a))
withAnyVersionFromM = withAnyVersion' @(from == v) @from @v @a @c @dec

-- | Pure version of 'withAnyVersionM'.
withAnyVersion
  :: forall v c a dec enc t
   . (WithAnyVersion v a c dec, c (a v), Alt t, Applicative t, Traversable t)
  => Decoder dec enc t a
  -> Apply a c
  -> enc
  -> t (Applied c a)
withAnyVersion dec action =
  runIdentity . withAnyVersionM @v @c @a dec (Identity . action)

-- | Pure version of 'withAnyVersionFromM'.
withAnyVersionFrom
  :: forall from v c a dec enc t
   . (WithAnyVersionFrom from v a c dec, c (a v), Alt t, Applicative t, Traversable t)
  => Decoder dec enc t a
  -> Apply a c
  -> enc
  -> t (Applied c a)
withAnyVersionFrom dec action =
  runIdentity . withAnyVersionFromM @from @v @c @a dec (Identity . action)

type DecodeAnyVersion v w a dec = DecodeAnyVersionFrom V0 v w a dec

type DecodeAnyVersionFrom from v w a dec = DecodeAnyVersion' (from == v) from v w a dec

class DecodeAnyVersion' (eq :: Bool) (from :: V) (v :: V) (w :: V) (a :: V -> Type) dec where
    decodeAnyVersion'
      :: (Alt t, Applicative t)
      => Decoder dec enc t a
      -> enc
      -> t (a w)

instance (from ~ v, dec (a from), Upgrade from w a)
  => DecodeAnyVersion' 'True from v w a dec where
    decodeAnyVersion' (Decoder decode) bs = upgrade @from @w <$> decode @from bs

instance (DecodeAnyVersion' (VPred v == from) from (VPred v) w a dec, dec (a v), dec (a (VPred v)), Upgrade v w a)
  => DecodeAnyVersion' 'False from v w a dec where
    decodeAnyVersion' decoder@(Decoder decode) bs = upgrade @v @w <$> decode @v bs
                                                <!> decodeAnyVersion' @(VPred v == from) @from @(VPred v) @w decoder bs

type WithAnyVersion v a c dec = WithAnyVersionFrom V0 v a c dec

type WithAnyVersionFrom from v a c dec = WithAnyVersion' (from == v) from v a c dec

class WithAnyVersion' (eq :: Bool) (from :: V) (v :: V) (a :: V -> Type) c dec where
    withAnyVersion' :: (Applicative m, Alt t, Applicative t, Traversable t, c (a v))
                    => Decoder dec enc t a
                    -> ApplyM m a c
                    -> enc
                    -> m (t (Applied c a))

instance (from ~ v, dec (a from), c (a from))
  => WithAnyVersion' 'True from v a c dec where
    withAnyVersion' (Decoder decode) action bs = traverse action (decode @from bs)

instance (WithAnyVersion' (VPred v == from) from (VPred v) a c dec, dec (a v), dec (a (VPred v)), c (a v), c (a (VPred v)))
  => WithAnyVersion' 'False from v a c dec where
    withAnyVersion' dec@(Decoder decode) action bs = do
        res  <- traverse action (decode @v bs)
        next <- withAnyVersion' @(VPred v == from) @from @(VPred v) @a @c dec action bs
        pure (res <!> next)
