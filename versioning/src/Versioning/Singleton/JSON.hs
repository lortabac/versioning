-- | Experimental singleton-based alternative to the 'Versioning.JSON' module
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE UndecidableInstances  #-}
module Versioning.Singleton.JSON where

import           Data.Aeson
import qualified Data.ByteString                        as StrictBS
import qualified Data.ByteString.Lazy                   as LazyBS

import           Versioning.JSON                        (jsonDecode,
                                                         jsonDecodeStrict,
                                                         jsonEitherDecode,
                                                         jsonEitherDecodeStrict)
import           Versioning.Singleton
import           Versioning.Singleton.Internal.Decoding

-- | Decode a JSON string by trying all the versions decrementally
--   and apply a pure function to the decoded object at its original version.
decodeJsonSomeVersion
  :: forall v a
   . (DecodeSomeVersion v a FromJSON)
  => LazyBS.ByteString
  -> Maybe (AtSomeV a)
decodeJsonSomeVersion = decodeSomeVersion @v @a jsonDecode

-- | Like 'decodeJsonSomeVersion' but it reads from a strict 'ByteString'
decodeJsonSomeVersionStrict
  :: forall v a
   . (DecodeSomeVersion v a FromJSON)
  => StrictBS.ByteString
  -> Maybe (AtSomeV a)
decodeJsonSomeVersionStrict = decodeSomeVersion @v @a jsonDecodeStrict

-- | Like 'decodeJsonSomeVersion' but returns a message when decoding fails
decodeJsonSomeVersionEither
  :: forall v a
   . (DecodeSomeVersion v a FromJSON)
  => LazyBS.ByteString
  -> Either String (AtSomeV a)
decodeJsonSomeVersionEither = decodeSomeVersion @v @a jsonEitherDecode

-- | Like 'decodeJsonSomeVersionStrict' but returns a message when decoding fails
decodeJsonSomeVersionEitherStrict
  :: forall v a
   . (DecodeSomeVersion v a FromJSON)
  => StrictBS.ByteString
  -> Either String (AtSomeV a)
decodeJsonSomeVersionEitherStrict = decodeSomeVersion @v @a jsonEitherDecodeStrict

-- | Like 'decodeJsonSomeVersion', with an additional type-parameter
--   indicating the oldest version you want to be able to decode
decodeJsonSomeVersionFrom
  :: forall from v a
   . (DecodeSomeVersionFrom from v a FromJSON)
  => LazyBS.ByteString
  -> Maybe (AtSomeV a)
decodeJsonSomeVersionFrom = decodeSomeVersionFrom @from @v @a jsonDecode
