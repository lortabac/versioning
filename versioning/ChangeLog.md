# Revision history

## 0.3.1.0

- Add `SinceS` to version sum types

- Add `downgrade` for version downcasting

## 0.3.0.0

- Encode `V` inductively

- Get rid of the overlapping instances (fixes a bug on GHC 8.2.1)

- Allow specifying the oldest supported version

- Switch to a multi-package cabal project

- Provide tools for working with Servant in the versioning-servant package

### Breaking changes

- `V` now starts at `V0` instead of `V1`

## 0.2.0.0

- Get the version number of a versioned value

- Decouple the decoding logic from the actual encoding

- Provide two families of functions for decoding JSON:
  `fromJsonAnyVersionX` and `withJsonAnyVersionX`

### Breaking changes

- `decodeAnyVersion` is now called `fromJsonAnyVersion`

- `withAnyVersion` is now called `withJsonAnyVersion`

- `withAnyVersionM` is now called `withJsonAnyVersionM`

## 0.1.0.0

- First version
