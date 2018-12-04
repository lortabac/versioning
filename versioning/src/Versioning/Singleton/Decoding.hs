-- | Encoding-agnostic singleton-based deserialization utilities.
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Versioning.Singleton.Decoding (
      WithAnyVersion
    , WithAnyVersionFrom
    , withAnyVersion
    , withAnyVersionM
    , withAnyVersionFrom
    , withAnyVersionFromM
) where

import           Data.Functor.Alt             (Alt (..))
import           Data.Functor.Identity        (Identity (..))
import           Data.Kind                    (Type)

import           Versioning.Base
import           Versioning.Internal.Decoding (Decoder (..))
import           Versioning.Internal.Equality (type (==))
import           Versioning.Singleton

-- | Decode by trying all the versions decrementally
--   and apply an action to the decoded object at its original version.
withAnyVersionM
  :: forall v a dec enc m t r
   . (WithAnyVersion v a dec, Alt t, Applicative t, Traversable t, Applicative m)
  => Decoder dec enc t a
  -> (forall w. SVI w => a w -> m r)
  -> enc
  -> m (t r)
withAnyVersionM = withAnyVersionFromM @V0 @v

-- | Like 'withAnyVersionM', with an additional type-parameter
--   indicating the oldest version you want to be able to decode
withAnyVersionFromM
  :: forall from v a dec enc m t r
   . (WithAnyVersionFrom from v a dec, Alt t, Applicative t, Traversable t, Applicative m)
  => Decoder dec enc t a
  -> (forall w. SVI w => a w -> m r)
  -> enc
  -> m (t r)
withAnyVersionFromM = withAnyVersion' @(from == v) @from @v @a @dec

-- | Pure version of 'withAnyVersionM'.
withAnyVersion
  :: forall v a dec enc t r
   . (WithAnyVersion v a dec, Alt t, Applicative t, Traversable t)
  => Decoder dec enc t a
  -> (forall w. SVI w => a w -> r)
  -> enc
  -> t r
withAnyVersion dec k =
  runIdentity . withAnyVersionM @v @a dec (Identity . k)

-- | Pure version of 'withAnyVersionFromM'.
withAnyVersionFrom
  :: forall from v a dec enc t r
   . (WithAnyVersionFrom from v a dec, Alt t, Applicative t, Traversable t)
  => Decoder dec enc t a
  -> (forall w. SVI w => a w -> r)
  -> enc
  -> t r
withAnyVersionFrom dec k =
  runIdentity . withAnyVersionFromM @from @v @a dec (Identity . k)

type WithAnyVersion v a dec = WithAnyVersionFrom V0 v a dec

type WithAnyVersionFrom from v a dec = WithAnyVersion' (from == v) from v a dec

class WithAnyVersion' (eq :: Bool) (from :: V) (v :: V) (a :: V -> Type) dec where
    withAnyVersion' :: (Applicative m, Alt t, Applicative t, Traversable t)
                    => Decoder dec enc t a
                    -> (forall w. SVI w => a w -> m r)
                    -> enc
                    -> m (t r)

instance (from ~ v, dec (a from), SVI v)
  => WithAnyVersion' 'True from v a dec where
    withAnyVersion' (Decoder decode) k bs = traverse (k @v) (decode @from bs)

instance (WithAnyVersion' (VPred v == from) from (VPred v) a dec, dec (a v), dec (a (VPred v)), SVI v, SVI (VPred v))
  => WithAnyVersion' 'False from v a dec where
    withAnyVersion' dec@(Decoder decode) k bs = do
        res  <- traverse (k @v) (decode @v bs)
        next <- withAnyVersion' @(VPred v == from) @from @(VPred v) @a dec k bs
        pure (res <!> next)
