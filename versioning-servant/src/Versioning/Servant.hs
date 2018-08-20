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
module Versioning.Servant
  ( VersionedJSON
  , VersionedJSONFrom
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
import           Data.Typeable                    (Typeable)
import qualified Network.HTTP.Media               as Media
import           Servant.API                      (Accept (..), MimeRender (..),
                                                   MimeUnrender (..))

import           Versioning.Base
import           Versioning.Internal.Decoding
import           Versioning.JSON

-- | Drop-in replacement for the 'JSON' data-type
--   for seamless integration with servant.
type VersionedJSON = VersionedJSONFrom V0

-- | Like 'VersionedJSON', with an additional type-parameter
--   indicating the oldest version you want to be able to decode
data VersionedJSONFrom (v :: V) deriving Typeable

instance Accept (VersionedJSONFrom from) where
    contentTypes _ =
      "application" Media.// "json" Media./: ("charset", "utf-8") NonEmpty.:|
      [ "application" Media.// "json" ]

-- We add a redundant 'JsonDecodableTo' constraint to minimize the risk
-- of using the 'VersionedJSON' type in the wrong place
instance {-# OVERLAPPABLE #-} (JsonDecodableToFrom from v a, ToJSON (a v))
  => MimeRender (VersionedJSONFrom from) (a v) where
    mimeRender _ = encode

instance JsonDecodableToFrom from v a => MimeUnrender (VersionedJSONFrom from) (a v) where
    mimeUnrender _ = fromJsonAnyVersionLenientFrom @from

-- | Like 'fromJsonAnyVersionEither', but it uses 'eitherDecodeLenient' for decoding
fromJsonAnyVersionLenientFrom
  :: forall from v a
   . JsonDecodableToFrom from v a
  => LazyBS.ByteString
  -> Either String (a v)
fromJsonAnyVersionLenientFrom = decodeAnyVersionFrom @from jsonDecodeLenient

-- | Lenient JSON decoder
jsonDecodeLenient
  :: Decoder FromJSON LazyBS.ByteString (Either String) (a :: V -> Type)
jsonDecodeLenient = Decoder eitherDecodeLenient

-- Copied and pasted from Servant.API.ContentTypes:

-- | Like 'Data.Aeson.eitherDecode' but allows all JSON values instead of just
-- objects and arrays.
--
-- Will handle trailing whitespace, but not trailing junk. ie.
--
-- >>> eitherDecodeLenient "1 " :: Either String Int
-- Right 1
--
-- >>> eitherDecodeLenient "1 junk" :: Either String Int
-- Left "trailing junk after valid JSON: endOfInput"
eitherDecodeLenient :: FromJSON a => LazyBS.ByteString -> Either String a
eitherDecodeLenient input = parseOnly parser (LazyBS.toStrict input)
  >>= parseEither parseJSON
 where
  parser =
    skipSpace
      *> value
      <* skipSpace
      <* (endOfInput <?> "trailing junk after valid JSON")
