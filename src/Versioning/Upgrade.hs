{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Versioning.Upgrade
  ( Adapt(..)
  , Upgrade(..)
  , upgrade
  )
where

import           Data.Kind       (Type)
import           GHC.TypeLits

import           Versioning.Base

-- | Adapt from a version to another
class Adapt (v :: V) (w :: V) (a :: V -> Type) where
    adapt :: a v -> a w

-- | Upgrade from a lower to a higher version by calling 'adapt' on all
--   the intermediary steps.
upgrade :: Upgrade v w a => a v -> a w
upgrade = upgrade'

-- | Upgrade from a lower to a higher version by calling 'adapt' on all
--   the intermediary steps.
--   You do not need to define any instance.
--   They are derived automatically if all the intermediary
--   'Adapt' instances are defined.
class Upgrade (v :: V) (w :: V) (a :: V -> Type) where
    upgrade' :: a v -> a w

instance {-# OVERLAPS #-} Upgrade v v a where
    upgrade' x = x

instance {-# OVERLAPPABLE #-} (Adapt v (Incr v w) a, Upgrade (Incr v w) w a)
  => Upgrade v w a where
    upgrade' x = upgrade' @(Incr v w) @w (adapt @v @(Incr v w) x)

-- | Increment version until the target version is reached
type family Incr (c :: V) (v :: V) :: V where
    Incr ('V c) ('V v) = Incr' (CmpNat c v) ('V c)

type family Incr' (o :: Ordering) (c :: V) :: V where
    Incr' 'LT ('V c) = 'V (c + 1)
    Incr' 'EQ ('V c) = 'V c
    Incr' 'GT ('V c) = TypeError
                       ( 'Text "Cannot increment "
                   ':<>: 'ShowType ('V c)
                   ':<>: 'Text " because the target is a lower version"
                       )
