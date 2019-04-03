-- | Experimental singleton definitions
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
module Versioning.Singleton
  (
    -- * Singleton data-type
    SV(..)
  , svPred
  , SVI(..)
  , SomeSV(..)
  , AtSomeV(..)
  , fromSV
  , toSomeSV
  , withSV
  , demoteV
    -- * Working at the term-level
  , vFromNum
  , vToNum
    -- * Convenience definitions
  , pattern SV0
  , pattern SV1
  , pattern SV2
  , pattern SV3
  , pattern SV4
  , pattern SV5
  , pattern SV6
  , pattern SV7
  , pattern SV8
  , pattern SV9
  , pattern SV10
  , pattern SV11
  , pattern SV12
  , pattern SV13
  , pattern SV14
  , pattern SV15
  , pattern SV16
  , pattern SV17
  , pattern SV18
  , pattern SV19
  , pattern SV20
  , pattern SVGreaterThan0
  , pattern SVGreaterThan1
  , pattern SVGreaterThan2
  , pattern SVGreaterThan3
  )
where

import           Data.Aeson         (ToJSON, toEncoding, toJSON)
import           Data.Kind          (Type)
import           Data.Type.Equality ((:~:) (..), TestEquality (..))
import           Numeric.Natural    (Natural)

import           Versioning.Base

-- | Singleton counterpart of 'V'
data SV (v :: V) where
    SVZero :: SV V0
    SVSucc :: SV v -> SV (VSucc v)

deriving instance Show (SV v)

instance TestEquality SV where
    testEquality SVZero SVZero = Just Refl
    testEquality SVZero _      = Nothing
    testEquality _ SVZero      = Nothing
    testEquality (SVSucc x) (SVSucc y) = do
        Refl <- testEquality x y
        pure Refl

-- | Get the previous version of a singleton.
svPred :: SV v -> SV (VPred v)
svPred SVZero     = SVZero
svPred (SVSucc s) = s

class SVI (v :: V) where
    sv :: SV v

instance SVI 'VZero where
    sv = SVZero

instance SVI v => SVI ('VSucc v) where
    sv = SVSucc (sv @v)

-- | Existentially-quantified 'SV'
data SomeSV = forall v. SomeSV (SV v)

instance Eq SomeSV where
    (SomeSV x) == (SomeSV y) = fromSV x == fromSV y

instance Ord SomeSV where
    compare (SomeSV x) (SomeSV y) = compare (fromSV x) (fromSV y)

mapSomeSVUp :: (forall v . SV v -> SV (VSucc v)) -> SomeSV -> SomeSV
mapSomeSVUp k (SomeSV x) = SomeSV (k x)

-- | A value at a given version
data AtSomeV (a :: V -> Type) where
    AtSomeV :: ToJSON (a v) => SV v -> a v -> AtSomeV a

instance ToJSON (AtSomeV a) where
    toJSON = toJSON
    toEncoding = toEncoding

-- | Convert an 'SV' to the corresponding 'V'
fromSV :: SV v -> V
fromSV SVZero     = VZero
fromSV (SVSucc s) = VSucc (fromSV s)

-- | Convert a 'V' to an existentially-quantified singleton
toSomeSV :: V -> SomeSV
toSomeSV VZero     = SomeSV SVZero
toSomeSV (VSucc v) = mapSomeSVUp SVSucc (toSomeSV v)

-- | Convert a 'V' to an 'SV', passing it into a continuation
withSV :: V -> (forall v . SV v -> r) -> r
withSV VZero     k = k SVZero
withSV (VSucc v) k = withSV v (k . SVSucc)

-- | Demote a 'V' from the type-level to the term-level.
--   You need @TypeApplications@ to use this function conveniently.
--   For example:
--
-- >>> demoteV @V2
-- VSucc (VSucc VZero)
demoteV :: forall v . SVI v => V
demoteV = fromSV (sv @v)

-- | Make a 'V' from a number
vFromNum :: Natural -> V
vFromNum = go VZero
  where
    go acc 0 = acc
    go acc n = go (VSucc acc) (n - 1)

-- | Get the version as a number
vToNum :: V -> Natural
vToNum = go 0
  where
    go acc VZero     = acc
    go acc (VSucc v) = go (acc + 1) v

pattern SV0 :: () => (v ~ V0) => SV v
pattern SV0 = SVZero

pattern SV1 :: () => (v ~ V1) => SV v
pattern SV1 = SVSucc SV0

pattern SV2 :: () => (v ~ V2) => SV v
pattern SV2 = SVSucc SV1

pattern SV3 :: () => (v ~ V3) => SV v
pattern SV3 = SVSucc SV2

pattern SV4 :: () => (v ~ V4) => SV v
pattern SV4 = SVSucc SV3

pattern SV5 :: () => (v ~ V5) => SV v
pattern SV5 = SVSucc SV4

pattern SV6 :: () => (v ~ V6) => SV v
pattern SV6 = SVSucc SV5

pattern SV7 :: () => (v ~ V7) => SV v
pattern SV7 = SVSucc SV6

pattern SV8 :: () => (v ~ V8) => SV v
pattern SV8 = SVSucc SV7

pattern SV9 :: () => (v ~ V9) => SV v
pattern SV9 = SVSucc SV8

pattern SV10 :: () => (v ~ V10) => SV v
pattern SV10 = SVSucc SV9

pattern SV11 :: () => (v ~ V11) => SV v
pattern SV11 = SVSucc SV10

pattern SV12 :: () => (v ~ V12) => SV v
pattern SV12 = SVSucc SV11

pattern SV13 :: () => (v ~ V13) => SV v
pattern SV13 = SVSucc SV12

pattern SV14 :: () => (v ~ V14) => SV v
pattern SV14 = SVSucc SV13

pattern SV15 :: () => (v ~ V15) => SV v
pattern SV15 = SVSucc SV14

pattern SV16 :: () => (v ~ V16) => SV v
pattern SV16 = SVSucc SV15

pattern SV17 :: () => (v ~ V17) => SV v
pattern SV17 = SVSucc SV16

pattern SV18 :: () => (v ~ V18) => SV v
pattern SV18 = SVSucc SV17

pattern SV19 :: () => (v ~ V19) => SV v
pattern SV19 = SVSucc SV18

pattern SV20 :: () => (v ~ V20) => SV v
pattern SV20 = SVSucc SV19

pattern SVGreaterThan0 :: () => (VGreaterThan V0 v) => SV v
pattern SVGreaterThan0 <- SVSucc _
{-# COMPLETE SV0, SVGreaterThan0 #-}

pattern SVGreaterThan1 :: () => (VGreaterThan V1 v) => SV v
pattern SVGreaterThan1 <- SVSucc (SVSucc _)
{-# COMPLETE SV0, SV1, SVGreaterThan1 #-}

pattern SVGreaterThan2 :: () => (VGreaterThan V2 v) => SV v
pattern SVGreaterThan2 <- SVSucc (SVSucc (SVSucc _))
{-# COMPLETE SV0, SV1, SV2, SVGreaterThan2 #-}

pattern SVGreaterThan3 :: () => (VGreaterThan V3 v) => SV v
pattern SVGreaterThan3 <- SVSucc (SVSucc (SVSucc (SVSucc _)))
{-# COMPLETE SV0, SV1, SV2, SV3, SVGreaterThan3 #-}
