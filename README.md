# Base32-lens

[![Build Status](https://travis-ci.com/emilypi/base32-lens.svg?branch=master)](https://travis-ci.com/emilypi/base32-lens)
[![Hackage](https://img.shields.io/hackage/v/base32-lens.svg)](https://hackage.haskell.org/package/base32-lens)

This package provides optics and convenient pattern synonyms for the [base32](https://hackage.haskell.org/package/base32) library.

### Patterns

The pattern synonyms provided in this library are:

```haskell
pattern Base32 :: ByteString -> ByteString
pattern Base32Url :: ByteString -> ByteString
pattern Base32UrlUnpadded :: ByteString -> ByteString

-- and

pattern Base32 :: Text -> Text
pattern Base32Url :: Text -> Text
pattern Base32UrlUnpadded :: Text -> Text
```

These provide a convenient high level interface for passing Base32 encoded values.


### Optics

`Prism`s for encoding and decoding `Text` and `ByteString` values are given as part of the library:


```haskell
_Base32 :: Prism' ByteString ByteString
_Base32Url :: Prism' ByteString ByteString
_Base32UrlUnpadded :: Prism' ByteString ByteString

-- and

_Base32 :: Prism' Text Text
_Base32Url :: Prism' Text Text
_Base32UrlUnpadded :: Prism' Text Text
```

If a particular structure has a `Lens` into some `Text` or `ByteString` value they might want to encode (or decode), then composing such a `Lens` with these `Prisms` yields an affine `Traversal`, resulting in a structure which has the focus of its `Lens` encoded as or decoded from Base32(-url).
