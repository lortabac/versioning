{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Versioning.Internal.Folding
  ( Decr
  , Incr
  )
where

import           GHC.TypeLits
import           GHC.TypeNats    (type (-), type (+))

import           Versioning.Base (V (..))

-- | Decrement version until the target version is reached
type family Decr (c :: V) (v :: V) :: V where
    Decr ('V c) ('V v) = Decr' (CmpNat c v) ('V c)

type family Decr' (o :: Ordering) (c :: V) :: V where
    Decr' 'GT ('V c) = 'V (c - 1)
    Decr' 'EQ ('V c) = 'V c
    Decr' 'LT ('V c) = TypeError
                       ( 'Text "Cannot decrement "
                   ':<>: 'ShowType ('V c)
                   ':<>: 'Text " because the target is a higher version"
                       )

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
