-- | This module provides some tools to encode multiple versions
--   of a data model in a single data-type parametrized by version number.
--   The addition or removal of a field can be expressed through the
--   'Since' and 'Until' type families.
--
--   Example:
--
-- > data Rec v = Rec
-- >     { foo :: Int               -- this field exists in all versions
-- >     , bar :: Since V2 v Bool   -- this field has been introduced in V2
-- >     , baz :: Until V2 v Double -- this field has been removed in V3
-- >     }
--
--   Besides reducing the number of data declarations,
--   this approach also has other advantages:
--
--   * It makes migrations declarative and self-documenting.
--
--   * It allows for less verbose version-upcasting functions,
--     since the fields that have a non-parametric type do not need to be copied.
--
--   * It is a foundation on which other useful abstractions can be built.
--
--   Please note that some classes may require a separate standalone deriving clause
--   for each version of a data-type or some kind of inductive deriving mechanism.
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Versioning.Base (
      -- * Types
      V (..)
    , VPred
    , VSucc
    , VNat
    , VCmp
    , Since
    , SinceS
    , Until
    , UntilS
    , NA
    , na
    , V0
    , V1
    , V2
    , V3
    , V4
    , V5
    , V6
    , V7
    , V8
    , V9
    , V10
    , V11
    , V12
    , V13
    , V14
    , V15
    , V16
    , V17
    , V18
    , V19
    , V20
      -- * Functions
    , versionNumber
) where

import           Data.Proxy               (Proxy (..))
import           GHC.TypeNats             (type (+), KnownNat, Nat, natVal)
import           Numeric.Natural          (Natural)

import           Versioning.Internal.Base (Bare)

-- | The version of a data model
data V = VZero | VSucc V deriving (Eq, Ord, Show)

-- | Get the previous version
type family VPred (v :: V) :: V where
    VPred ('VSucc v) = v
    VPred 'VZero = 'VZero

type VSucc = 'VSucc

-- | Get the type-level natural of a version
type family VNat (v :: V) :: Nat where
    VNat v = VNat' v 0

type family VNat' (v :: V) (n :: Nat) :: Nat where
    VNat' 'VZero n = n
    VNat' ('VSucc v) n = VNat' v (n + 1)

-- | Compare two versions
type family VCmp (v :: V) (w :: V) :: Ordering where
    VCmp 'VZero 'VZero = 'EQ
    VCmp 'VZero v = 'LT
    VCmp v 'VZero = 'GT
    VCmp ('VSucc v) ('VSucc w) = VCmp v w

-- | Get the version number of a versioned value
versionNumber :: forall a v. KnownNat (VNat v) => a v -> Natural
versionNumber _ = natVal (Proxy :: Proxy (VNat v))

-- | This allows us to express that a field is only present since
--   a given version.
--   The first parameter is the version in which the field has been introduced,
--   the second parameter is the actual version of the data-type.
type family Since (s :: V) (v :: V) a where
    Since s v a = Since' (VCmp s v) a NA

-- | Same as 'Since', for sum types.
--   The only difference between 'Since' and 'SinceS' is in the type used to
--   indicate absence.
--   In 'Since' absence is expressed with 'NA', which is isomorphic to '()'.
--   In 'SinceS' it is expressed with 'Bare', which is isomorphic to 'Void'.
type family SinceS (s :: V) (v :: V) a where
    SinceS s v a = Since' (VCmp s v) a Bare

type family Since' (o :: Ordering) a absent :: * where
    Since' 'LT a _ = a
    Since' 'EQ a _ = a
    Since' 'GT a absent = absent

-- | This allows us to express that a field is only present until
--   a given version.
--   The first parameter is the last version in which the field is present,
--   the second parameter is the actual version of the data-type.
type family Until (u :: V) (v :: V) a where
    Until u v a = Until' (VCmp u v) a NA

-- | Same as 'Until', for sum types.
type family UntilS (u :: V) (v :: V) a where
    UntilS u v a = Until' (VCmp u v) a Bare

type family Until' (o :: Ordering) a absent :: * where
    Until' 'GT a _ = a
    Until' 'EQ a _ = a
    Until' 'LT a absent = absent

-- | A type indicating absence.
--   The 'Maybe' is a hack needed to let aeson parse a record successfully even
--   if a field of type 'NA' is missing.
--
--   Ideally we would like to define it as
--
-- > data NA = NA
--
--   but this would not work with 'FromJSON' instances that are derived
--   with Generic.
type NA = Maybe Bare

-- | A placeholder for an absent value.
na :: NA
na = Nothing

-- Version type synonyms
type V0 = 'VZero

type V1 = 'VSucc V0

type V2 = 'VSucc V1

type V3 = 'VSucc V2

type V4 = 'VSucc V3

type V5 = 'VSucc V4

type V6 = 'VSucc V5

type V7 = 'VSucc V6

type V8 = 'VSucc V7

type V9 = 'VSucc V8

type V10 = 'VSucc V9

type V11 = 'VSucc V10

type V12 = 'VSucc V11

type V13 = 'VSucc V12

type V14 = 'VSucc V13

type V15 = 'VSucc V14

type V16 = 'VSucc V15

type V17 = 'VSucc V16

type V18 = 'VSucc V17

type V19 = 'VSucc V18

type V20 = 'VSucc V19
