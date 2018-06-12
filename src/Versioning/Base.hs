-- | This module provides some tools to encode multiple versions
--   of a data model in a single data-type parametrized by version number.
--   The addition or removal of a field can be expressed through the
--   'Since' and 'Until' type families.
--
--   Example:
--   data Rec v = Rec
--       { foo :: Int               -- this field exists in all versions
--       , bar :: Since V2 v Bool   -- this field has been introduced in V2
--       , baz :: Until V2 v Double -- this field has been removed in V3
--       }
--
--   Besides reducing the number of data declarations,
--   this approach also has other advantages:
--   * It makes migrations declarative and self-documenting.
--   * It allows for less verbose version-upcasting functions,
--     since the fields that have a non-parametric type do not need to be copied.
--   * It is a foundation on which other useful abstractions can be built.
--   However there are also some drawbacks to consider:
--   some classes may require a separate standalone deriving clause for each version.
--   Moreover the usage of type-level computations can make the error messages
--   harder to understand.
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Versioning.Base
  ( V(..)
  , GetV
  , versionNumber
  , Since
  , Until
  , NA
  , na
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
  )
where

import           Data.Proxy               (Proxy (..))
import           GHC.TypeNats             (type (-), KnownNat, Nat, natVal)
import           Numeric.Natural          (Natural)

import           Versioning.Internal.Base (Bare)

-- | The version of a data model
newtype V = V Nat

-- | Get the type-level natural of a version
type family GetV (v :: V) :: Nat where
    GetV ('V n) = n

-- | Get the version number of a versioned value
versionNumber :: forall a v. KnownNat (GetV v) => a v -> Natural
versionNumber _ = natVal (Proxy :: Proxy (GetV v))

-- | This allows us to express that a field is only present since
--   a given version.
--   The first parameter is the version in which the field has been introduced,
--   the second parameter is the actual version of the data-type.
type family Since (s :: V) (v :: V) a :: * where
    Since ('V 1) ('V v) a = a
    Since ('V s) ('V 1) a = NA
    Since ('V s) ('V v) a = Since ('V (s - 1)) ('V (v - 1)) a

-- | This allows us to express that a field is only present until
--   a given version.
--   The first parameter is the last version in which the field is present,
--   the second parameter is the actual version of the data-type.
type family Until (u :: V) (v :: V) a :: * where
    Until ('V u) ('V 1) a = a
    Until ('V 1) ('V v) a = NA
    Until ('V u) ('V v) a = Until ('V (u - 1)) ('V (v - 1)) a

-- | A type indicating absence.
--   The 'Maybe' is a hack needed to let aeson parse a record even
--   if a field is missing.
type NA = Maybe Bare

-- | A placeholder for an absent value.
na :: NA
na = Nothing

-- Handy type synonyms to minimize parenthesis
type V1 = 'V 1

type V2 = 'V 2

type V3 = 'V 3

type V4 = 'V 4

type V5 = 'V 5

type V6 = 'V 6

type V7 = 'V 7

type V8 = 'V 8

type V9 = 'V 9

type V10 = 'V 10

type V11 = 'V 11

type V12 = 'V 12

type V13 = 'V 13

type V14 = 'V 14

type V15 = 'V 15

type V16 = 'V 16

type V17 = 'V 17

type V18 = 'V 18

type V19 = 'V 19

type V20 = 'V 20
