-- | Experimental singleton-based alternative to the 'Versioning.Internal.Decoding' module
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ApplicativeDo          #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Versioning.Singleton.Internal.Decoding (
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
import           Data.Type.Bool

import           Versioning.Base
import           Versioning.Internal.Base
import           Versioning.Internal.Decoding (Decoder (..))
import           Versioning.Internal.Equality (type (==))
import           Versioning.Singleton

type family DecodeResult (supported :: [V]) (v :: V) (a :: Type) where
    DecodeResult vs v a = If (Elem vs v) (Decoded a) (Unsupported a)

newtype Decoded a = Decoded a

data Unsupported a = Unsupported

type family Elem (as :: [k]) (a :: k) :: Bool where
    Elem (a : _) a = 'True
    Elem (b : as) a = Elem as a
    Elem '[] _ = 'False

-- | Decode by trying all the versions decrementally
--   and apply an action to the decoded object at its original version.
withAnyVersionM
  :: forall (supported :: [V]) (v :: V) a dec enc m t r
   . (WithAnyVersion supported v a dec r, Alt t, Applicative t, Traversable t, Applicative m)
  => Decoder dec enc t a
  -> (forall w. SVI w => a w -> m (DecodeResult supported v r))
  -> enc
  -> m (t (DecodeResult supported v r))
withAnyVersionM = withAnyVersionFromM @V0 @supported @v

-- | Like 'withAnyVersionM', with an additional type-parameter
--   indicating the oldest version you want to be able to decode
withAnyVersionFromM
  :: forall from (supported :: [V]) (v :: V) a dec enc m t r
   . (WithAnyVersionFrom from supported v a dec r, Alt t, Applicative t, Traversable t, Applicative m)
  => Decoder dec enc t a
  -> (forall w. SVI w => a w -> m (DecodeResult supported v r))
  -> enc
  -> m (t (DecodeResult supported v r))
withAnyVersionFromM = withAnyVersion' @(from == v) @from @supported @v @a @dec

-- | Pure version of 'withAnyVersionM'.
withAnyVersion
  :: forall (supported :: [V]) (v :: V) a dec enc t r
   . (WithAnyVersion supported v a dec r, Alt t, Applicative t, Traversable t)
  => Decoder dec enc t a
  -> (forall w. SVI w => a w -> DecodeResult supported v r)
  -> enc
  -> t (DecodeResult supported v r)
withAnyVersion dec k =
  runIdentity . withAnyVersionM @supported @v @a dec (Identity . k)

-- | Pure version of 'withAnyVersionFromM'.
withAnyVersionFrom
  :: forall from (supported :: [V]) (v :: V) a dec enc t r
   . (WithAnyVersionFrom from supported v a dec r, Alt t, Applicative t, Traversable t)
  => Decoder dec enc t a
  -> (forall w. SVI w => a w -> DecodeResult supported v r)
  -> enc
  -> t (DecodeResult supported v r)
withAnyVersionFrom dec k =
  runIdentity . withAnyVersionFromM @from @supported @v @a dec (Identity . k)

type WithAnyVersion supported v a dec r = WithAnyVersionFrom V0 supported v a dec r

type WithAnyVersionFrom from supported v a dec r = WithAnyVersion' (from == v) from supported v a dec r

class WithAnyVersion' (eq :: Bool) (from :: V) (supported :: [V]) (v :: V) (a :: V -> Type) dec r where
    withAnyVersion' :: (Applicative m, Alt t, Applicative t, Traversable t)
                    => Decoder dec enc t a
                    -> (forall w. SVI w => a w -> m (DecodeResult supported v r))
                    -> enc
                    -> m (t (DecodeResult supported v r))

instance (from ~ v, dec (a from), SVI v)
  => WithAnyVersion' 'True from supported v a dec r where
    withAnyVersion' (Decoder decode) k bs = traverse (k @v) (decode @from bs)

instance (WithAnyVersion' (VPred v == from) from supported (VPred v) a dec r, dec (a v), dec (a (VPred v)), SVI v, SVI (VPred v))
  => WithAnyVersion' 'False from supported v a dec r where
    withAnyVersion' dec@(Decoder decode) k bs = do
        res  <- traverse (k @v) (decode @v bs)
        next <- withAnyVersion' @(VPred v == from) @from @supported @(VPred v) @a dec k bs
        pure (res <!> next)
