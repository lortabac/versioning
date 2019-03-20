{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Versioning.Servant.Dependent
  ( EncodedJSON
  , EncodedJSONFrom
  )
where

import           Data.Aeson                       (FromJSON, ToJSON, encode,
                                                   parseJSON)
import           Data.Aeson.Parser                (value)
import           Data.Aeson.Types                 (parseEither)
import           Data.Attoparsec.ByteString.Char8 (endOfInput, parseOnly,
                                                   skipSpace, (<?>))
import qualified Data.ByteString.Lazy             as LazyBS
import           Data.Kind                        (Type)
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Text                        as Text
import           Data.Typeable                    (Typeable)
import qualified Network.HTTP.Media               as Media
import           Numeric.Natural                  (Natural)
import           Servant.API
import           Servant.API.ContentTypes         (eitherDecodeLenient)
import           Versioning.Base
import           Versioning.Internal.Decoding
import           Versioning.JSON
import           Versioning.Singleton

-- | Drop-in replacement for the 'JSON' data-type
--   for seamless integration with servant.
type EncodedJSON = EncodedJSONFrom V0

-- | Like 'EncodedJSON', with an additional type-parameter
--   indicating the oldest version you want to be able to decode
data EncodedJSONFrom (v :: V) deriving Typeable

instance Accept (EncodedJSONFrom from) where
    contentTypes _ =
      "application" Media.// "json" Media./: ("charset", "utf-8") NonEmpty.:|
      [ "application" Media.// "json" ]

-- We add a redundant 'JsonDecodableTo' constraint to minimize the risk
-- of using the 'EncodedJSON' type in the wrong place
instance {-# OVERLAPPABLE #-} ToJSON (AtSomeV a) => MimeRender (EncodedJSONFrom from) (AtSomeV a) where
    mimeRender _ = encode

instance FromHttpApiData V where
    parseUrlPiece x = case Text.uncons x of
        Just ('v', n) -> vFromNum <$> parseUrlPiece @Natural n
        _             -> Left "Invalid version"

instance ToHttpApiData V where
    toUrlPiece = Text.cons 'v' <$> (toUrlPiece . vToNum)

instance FromHttpApiData SomeSV where
    parseUrlPiece x = toSomeSV <$> parseUrlPiece @V x
