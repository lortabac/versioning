# versioning

This package provides various tools to deal with data versioning in a type-safe way.

The base idea is that multiple versions of a data-model can be encoded in a single type parametrized by a version number.
The addition or removal of a field can be expressed through the `Since` and `Until` type families.

Example:

```haskell
data Rec v = Rec
    { foo :: Int               -- this field exists in all versions
    , bar :: Since V2 v Bool   -- this field has been introduced in V2
    , baz :: Until V2 v Double -- this field has been removed in V3
    }
```

### Upcasting

Thanks to this encoding we gain the ability to iterate on a range of versions.

For example, once we define how to `adapt` from each version to the next,
we can `upgrade` across multiple versions automatically:

```haskell
instance Adapt V1 V2 Rec where
    adapt rec = rec
        { bar = False   -- this field is new in V2, we set it to a default value
        , baz = baz rec
        }

instance Adapt V2 V3 Rec where
    adapt rec = rec
        { bar = bar rec
        , baz = na      -- this field is not available anymore in V3
        }

-- | Upgrade a 'Rec' from V1 to V3
upgradeRec :: Rec V1 -> Rec V3
upgradeRec = upgrade
```

### JSON decoding

If we know how to decode each version from JSON,
we can decode a JSON string by trying all the versions
from the newest to the oldest until we succeed,
and upgrade the decoded object to the latest version.

This is all done automatically thanks to the `DecodeAnyVersion` class.

```haskell
decodeRec :: ByteString -> Maybe (Rec V3)
decodeRec = fromJsonAnyVersion
```

#### Using a decoded object

Sometimes, instead of upgrading,
we need to use the decoded object at its original version.

In order to do this we have to provide a constraint that can be solved
with any version up to the most recent one.

We can then use the `WithAnyVersion` class to decode from JSON and apply a function
to the decoded object.

```haskell
-- Specify the return type of the function we want to apply
type instance Applied Show Rec = String

-- | Decode a 'Rec' of any version and return its string representation.
--   It requires 'Show' instances for all versions of 'Foo' up to V3.
decodeRecAndShow :: ByteString -> Maybe String
decodeRecAndShow = withJsonAnyVersion @Show @Rec @V3 show
```

## Inspiration

The `Since` type family is a suggestion that late Ertugrul Soylemez's (a.k.a. ertes) gave me on IRC.
The rest is just an elaboration on that base idea.

## License

    Copyright 2018 Lorenzo Tabacchini

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
