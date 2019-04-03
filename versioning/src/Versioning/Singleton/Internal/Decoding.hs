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
      DecodeSomeVersion
    , DecodeSomeVersionFrom
    , decodeSomeVersion
    , decodeSomeVersionFrom
) where

import           Data.Aeson                   (ToJSON)
import           Data.Functor.Alt             (Alt (..))
import           Data.Kind                    (Type)

import           Versioning.Base
import           Versioning.Internal.Decoding (Decoder (..))
import           Versioning.Internal.Equality (type (==))
import           Versioning.Singleton

-- | Decode by trying all the versions decrementally
decodeSomeVersion
  :: forall (v :: V) a dec enc t
   . (DecodeSomeVersion v a dec, Alt t)
  => Decoder dec enc t a
  -> enc
  -> t (AtSomeV a)
decodeSomeVersion = decodeSomeVersionFrom @V0 @v

-- | Like 'decodeSomeVersionM', with an additional type-parameter
--   indicating the oldest version you want to be able to decode
decodeSomeVersionFrom
  :: forall from (v :: V) a dec enc t
   . (DecodeSomeVersionFrom from v a dec, Alt t)
  => Decoder dec enc t a
  -> enc
  -> t (AtSomeV a)
decodeSomeVersionFrom = decodeSomeVersion' @(from == v) @from @v @a @dec

type DecodeSomeVersion v a dec = DecodeSomeVersionFrom V0 v a dec

type DecodeSomeVersionFrom from v a dec = DecodeSomeVersion' (from == v) from v a dec

class DecodeSomeVersion' (eq :: Bool) (from :: V) (v :: V) (a :: V -> Type) dec where
    decodeSomeVersion' :: (Alt t)
                       => Decoder dec enc t a
                       -> enc
                       -> t (AtSomeV a)

instance (from ~ v, dec (a from), SVI v, ToJSON (a v))
  => DecodeSomeVersion' 'True from v a dec where
    decodeSomeVersion' (Decoder decode) bs = AtSomeV sv <$> decode @from bs

instance (DecodeSomeVersion' (VPred v == from) from (VPred v) a dec, dec (a v), dec (a (VPred v)), SVI v, SVI (VPred v), ToJSON (a v), ToJSON (a (VPred v)))
  => DecodeSomeVersion' 'False from v a dec where
    decodeSomeVersion' dec@(Decoder decode) bs = (AtSomeV sv <$> res) <!> next
      where
        res = decode @v bs
        next = decodeSomeVersion' @(VPred v == from) @from @(VPred v) @a dec bs
