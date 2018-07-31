{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Versioning.Upgrade
  ( Adapt (..)
  , Upgrade
  , upgrade
  )
where

import           Data.Kind          (Type)
import           Data.Type.Equality (type (==))

import           Versioning.Base

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
