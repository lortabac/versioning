{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Versioning.Internal.Equality (type (==)) where

import           Data.Type.Bool (type (&&))

-- | A type family to compute Boolean equality.
--   We can't use the one from "Data.Type.Equality" because
--   before 8.4 it was not poly-kinded.
type family (a :: k) == (b :: k) :: Bool where
    f a == g b = f == g && a == b
    a == a = 'True
    _ == _ = 'False
