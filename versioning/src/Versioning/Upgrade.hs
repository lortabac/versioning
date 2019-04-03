{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE ExplicitNamespaces      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Versioning.Upgrade
  ( Adapt (..)
    -- * Upgrading
  , Upgrade
  , upgrade
    -- * Downgrading
  , Downgrade
  , downgrade
  )
where

import           Data.Kind                    (Type)

import           Versioning.Base
import           Versioning.Internal.Equality (type (==))

-- | Adapt from a version to another
class Adapt (v :: V) (w :: V) (a :: V -> Type) where
    adapt :: a v -> a w

-- | Upgrade from a lower to a higher version by calling 'adapt' on all
--   the intermediary steps.
upgrade :: forall v w a. Upgrade v w a => a v -> a w
upgrade = upgrade' @(v == w)

-- | This constraint specifies that a value of type 'a' can be upgraded
--   from version 'v' to version 'w'.
type Upgrade v w a = Upgrade' (v == w) v w a

-- | Upgrade from a lower to a higher version by calling 'adapt' on all
--   the intermediary steps.
--   You do not need to define any instance.
--   They are derived automatically if all the intermediary
--   'Adapt' instances are defined.
class Upgrade' (eq :: Bool) (v :: V) (w :: V) (a :: V -> Type) where
    upgrade' :: a v -> a w

instance (v ~ w) => Upgrade' 'True v w a where
    upgrade' x = x

instance (Adapt v (VSucc v) a, Upgrade' (VSucc v == w) (VSucc v) w a)
  => Upgrade' 'False v w a where
    upgrade' x = upgrade' @(VSucc v == w) @(VSucc v) @w (adapt @v @(VSucc v) x)

-- | Downgrade from a higher to a lower version by calling 'adapt' on all
--   the intermediary steps.
downgrade :: forall v w a. Downgrade v w a => a v -> a w
downgrade = downgrade' @(v == w)

-- | This constraint specifies that a value of type 'a' can be downgraded
--   from version 'v' to version 'w'.
type Downgrade v w a = Downgrade' (v == w) v w a

-- | Downgrade from a higher to a lower version by calling 'adapt' on all
--   the intermediary steps.
--   You do not need to define any instance.
--   They are derived automatically if all the intermediary
--   'Adapt' instances are defined.
class (VLTEQ v w) => Downgrade' (eq :: Bool) (v :: V) (w :: V) (a :: V -> Type) where
    downgrade' :: a v -> a w

instance (v ~ w, VLTEQ w w) => Downgrade' 'True v w a where
    downgrade' x = x

instance (Adapt v (VPred v) a, Downgrade' (VPred v == w) (VPred v) w a, VLTEQ v w)
  => Downgrade' 'False v w a where
    downgrade' x = downgrade' @(VPred v == w) @(VPred v) @w (adapt @v @(VPred v) x)
