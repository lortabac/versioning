-- | Encoding-agnosting deserialization utilities.
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ApplicativeDo         #-}
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
module Versioning.Internal.Decoding
  ( Applied
  , Apply
  , ApplyM
  , DecodableTo
  , DecodeAnyVersion
  , Decoder (..)
  , WithAnyVersion
  , decodeAnyVersion
  , withAnyVersion
  , withAnyVersionM
  )
where

import           Data.Functor.Alt            (Alt (..))
import           Data.Functor.Identity       (Identity (..))
import           Data.Kind                   (Constraint, Type)

import           Versioning.Base
import           Versioning.Internal.Folding (Decr)
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
type DecodableTo dec v a = DecodeAnyVersion v v a dec

-- | Decode by trying all the versions decrementally
--   and upgrade the decoded object to the newest version.
decodeAnyVersion
  :: forall v a dec enc t
   . (Alt t, Applicative t, DecodableTo dec v a)
  => Decoder dec enc t a
  -> enc
  -> t (a v)
decodeAnyVersion = decodeAnyVersion' @v @v @a @dec

-- | Decode by trying all the versions decrementally
--   and apply an action to the decoded object at its original version.
withAnyVersionM
  :: forall v c a dec enc m t
   . (WithAnyVersion v a c dec, Alt t, Applicative t, Traversable t, Applicative m, c (a v))
  => Decoder dec enc t a
  -> ApplyM m a c
  -> enc
  -> m (t (Applied c a))
withAnyVersionM = withAnyVersion' @v @a @c @dec

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

class DecodeAnyVersion (v :: V) (w :: V) (a :: V -> Type) dec where
    decodeAnyVersion'
      :: (Alt t, Applicative t)
      => Decoder dec enc t a
      -> enc
      -> t (a w)

instance {-# OVERLAPPING #-} (dec (a V1), Upgrade V1 w a)
  => DecodeAnyVersion V1 w a dec where
    decodeAnyVersion' (Decoder decode) bs = upgrade @V1 @w <$> decode @V1 bs

instance {-# OVERLAPPABLE #-} (DecodeAnyVersion (Decr v V1) w a dec, dec (a v), dec (a (Decr v V1)), Upgrade v w a)
  => DecodeAnyVersion v w a dec where
    decodeAnyVersion' decoder@(Decoder decode) bs = upgrade @v @w <$> decode @v bs
                                                <!> decodeAnyVersion' @(Decr v V1) @w decoder bs

class WithAnyVersion (v :: V) (a :: V -> Type) c dec where
    withAnyVersion' :: (Applicative m, Alt t, Applicative t, Traversable t, c (a v))
                    => Decoder dec enc t a
                    -> ApplyM m a c
                    -> enc
                    -> m (t (Applied c a))

instance {-# OVERLAPPING #-} (dec (a V1), c (a V1))
  => WithAnyVersion V1 a c dec where
    withAnyVersion' (Decoder decode) action bs = traverse action (decode @V1 bs)

instance {-# OVERLAPPABLE #-} (WithAnyVersion (Decr v V1) a c dec, dec (a v), dec (a (Decr v V1)), c (a v), c (a (Decr v V1)))
  => WithAnyVersion v a c dec where
    withAnyVersion' dec@(Decoder decode) action bs = do
        res  <- traverse action (decode @v bs)
        next <- withAnyVersion' @(Decr v V1) @a @c dec action bs
        pure (res <!> next)
