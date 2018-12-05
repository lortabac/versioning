{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Versioning.Servant.Dependent where

import           Data.Kind                                  (Constraint, Type)
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  as Text
import qualified Data.Text                                  as Text
import           GHC.TypeLits                               (KnownSymbol,
                                                             Symbol, symbolVal)
import           Numeric.Natural                            (Natural)
import           Servant.API                                ((:<|>), (:>),
                                                             Capture,
                                                             FromHttpApiData (..),
                                                             Get, JSON,
                                                             ToHttpApiData (..))
import           Servant.Server                             (Context,
                                                             HasServer (..),
                                                             Server, err400)
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication
import           Web.HttpApiData                            (parseUrlPieceMaybe)

import           Versioning.Base
import           Versioning.Singleton

-- | Existential wrapper
data Some :: (k -> Type) -> Type where
    Some :: f a -> Some f

-- | Constraint reification
data Dict (c :: Constraint) where
    Dict :: c => Dict c

-- | Type-level application
type family Apply (f :: s) (a :: k) :: Type

-- | Server dependent on some index
newtype DepServer (ix :: k -> Type) (f :: s) (m :: Type -> Type) =
    DepServer (forall a. ix a -> ServerT (Apply f a) m)

-- | Dependent analogue of 'HasServer'
class HasDepServer ix f where
    hasDepServer :: Proxy f -> ix a -> Dict (HasServer (Apply f a) ctx)

-- | Dependent capture on some index @ix@
data DepCapture (ix :: k -> Type) (f :: s)

instance (FromHttpApiData (Some ix), HasDepServer ix f)
  => HasServer (DepCapture ix f) ctx where
    type ServerT (DepCapture ix f) m = DepServer ix f m

    -- FIXME What is 'a'?
    hoistServerWithContext _ pc nt (DepServer s) = DepServer (hoistServerWithContext (Proxy :: Proxy (Apply f a)) pc nt . s)

    -- FIXME 'a' is ambiguous.
    -- In servant-server-0.4 you could first parse the next segment,
    -- do dependent pattern-matching on the result,
    -- and __then__ call 'route' recursively inside the matched case,
    -- but this is impossible now, so I need to find another solution.
    route Proxy ctx d = CaptureRouter $
        route (Proxy :: Proxy (Apply f a))
              ctx
              (addDepCapture d $ \txt -> case parseUrlPieceMaybe @(Some ix) txt of
                 Nothing -> delayedFail err400
                 Just (Some (x :: ix a)) -> case hasDepServer (Proxy :: Proxy f) x of
                     Dict -> return x
              )

-- | Add a dependent capture to the end of the capture block.
addDepCapture :: Delayed env (DepServer ix f m)
              -> (captured -> DelayedIO (ix a))
              -> Delayed (captured, env) (ServerT (Apply f a) m)
addDepCapture Delayed{..} new = Delayed
    { capturesD = \(txt, env) -> (,) <$> capturesD env <*> new txt
    , serverD   = \(x, v) p h a b req -> (\(DepServer k) -> k v) <$> serverD x p h a b req
    , ..
    }

-------------------------------------------------------------------------------
-- Orphan instances for 'V' and 'SV'
-------------------------------------------------------------------------------

instance FromHttpApiData V where
    parseUrlPiece x = case Text.uncons x of
        Just ('v', n) -> vFromNum <$> parseUrlPiece @Natural n
        _             -> Left "Invalid version"

instance ToHttpApiData V where
    toUrlPiece = Text.cons 'v' <$> (toUrlPiece . vToNum)

instance FromHttpApiData SomeSV where
    parseUrlPiece x = toSomeSV <$> parseUrlPiece @V x

instance FromHttpApiData (Some SV) where
    parseUrlPiece x = toGenericSome <$> parseUrlPiece @SomeSV x

toGenericSome :: SomeSV -> Some SV
toGenericSome (SomeSV v) = Some v
