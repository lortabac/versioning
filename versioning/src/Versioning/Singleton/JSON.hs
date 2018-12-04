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
import qualified Data.ByteString               as StrictBS
import qualified Data.ByteString.Lazy          as LazyBS

import           Versioning.JSON               (jsonDecode, jsonDecodeStrict,
                                                jsonEitherDecode,
                                                jsonEitherDecodeStrict)
import           Versioning.Singleton          (SVI)
import           Versioning.Singleton.Decoding

-- | Decode a JSON string by trying all the versions decrementally
--   and apply an action to the decoded object at its original version.
withJsonAnyVersionM
  :: forall a v m r
   . (WithAnyVersion v a FromJSON, Applicative m)
  => (forall w. SVI w => a w -> m r)
  -> LazyBS.ByteString
  -> m (Maybe r)
withJsonAnyVersionM = withAnyVersionM @v @a jsonDecode

-- | Like 'withJsonAnyVersionM' but it reads from a strict 'ByteString'
withJsonAnyVersionStrictM
  :: forall a v m r
   . (WithAnyVersion v a FromJSON, Applicative m)
  => (forall w. SVI w => a w -> m r)
  -> StrictBS.ByteString
  -> m (Maybe r)
withJsonAnyVersionStrictM = withAnyVersionM @v @a jsonDecodeStrict

-- | Like 'withJsonAnyVersionM' but returns a message when decoding fails
withJsonAnyVersionEitherM
  :: forall a v m r
   . (WithAnyVersion v a FromJSON, Applicative m)
  => (forall w. SVI w => a w -> m r)
  -> LazyBS.ByteString
  -> m (Either String r)
withJsonAnyVersionEitherM = withAnyVersionM @v @a jsonEitherDecode

-- | Like 'withJsonAnyVersionStrictM' but returns a message when decoding fails
withJsonAnyVersionEitherStrictM
  :: forall a v m r
   . (WithAnyVersion v a FromJSON, Applicative m)
  => (forall w. SVI w => a w -> m r)
  -> StrictBS.ByteString
  -> m (Either String r)
withJsonAnyVersionEitherStrictM =
  withAnyVersionM @v @a jsonEitherDecodeStrict

-- | Like 'withJsonAnyVersionM', with an additional type-parameter
--   indicating the oldest version you want to be able to decode
withJsonAnyVersionFromM
  :: forall from a v m r
   . (WithAnyVersionFrom from v a FromJSON, Applicative m)
  => (forall w. SVI w => a w -> m r)
  -> LazyBS.ByteString
  -> m (Maybe r)
withJsonAnyVersionFromM = withAnyVersionFromM @from @v @a jsonDecode

-- | Decode a JSON string by trying all the versions decrementally
--   and apply a pure function to the decoded object at its original version.
withJsonAnyVersion
  :: forall a v r
   . (WithAnyVersion v a FromJSON)
  => (forall w. SVI w => a w -> r)
  -> LazyBS.ByteString
  -> Maybe r
withJsonAnyVersion = withAnyVersion @v @a jsonDecode

-- | Like 'withJsonAnyVersion' but it reads from a strict 'ByteString'
withJsonAnyVersionStrict
  :: forall a v r
   . (WithAnyVersion v a FromJSON)
  => (forall w. SVI w => a w -> r)
  -> StrictBS.ByteString
  -> Maybe r
withJsonAnyVersionStrict = withAnyVersion @v @a jsonDecodeStrict

-- | Like 'withJsonAnyVersion' but returns a message when decoding fails
withJsonAnyVersionEither
  :: forall a v r
   . (WithAnyVersion v a FromJSON)
  => (forall w. SVI w => a w -> r)
  -> LazyBS.ByteString
  -> Either String r
withJsonAnyVersionEither = withAnyVersion @v @a jsonEitherDecode

-- | Like 'withJsonAnyVersionStrict' but returns a message when decoding fails
withJsonAnyVersionEitherStrict
  :: forall a v r
   . (WithAnyVersion v a FromJSON)
  => (forall w. SVI w => a w -> r)
  -> StrictBS.ByteString
  -> Either String r
withJsonAnyVersionEitherStrict = withAnyVersion @v @a jsonEitherDecodeStrict

-- | Like 'withJsonAnyVersion', with an additional type-parameter
--   indicating the oldest version you want to be able to decode
withJsonAnyVersionFrom
  :: forall from a v r
   . (WithAnyVersionFrom from v a FromJSON)
  => (forall w. SVI w => a w -> r)
  -> LazyBS.ByteString
  -> Maybe r
withJsonAnyVersionFrom = withAnyVersionFrom @from @v @a jsonDecode
