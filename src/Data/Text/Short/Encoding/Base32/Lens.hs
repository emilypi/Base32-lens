{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#if MIN_VERSION_lens(5,0,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Module       : Data.Text.Short.Encoding.Base32.Lens
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: non-portable
--
-- This module contains 'Prism's Base32-encoding and
-- decoding 'Text' values.
--
module Data.Text.Short.Encoding.Base32.Lens
( -- * Prisms
  _Base32
, _Base32Unpadded
, _Base32Hex
, _Base32HexUnpadded
-- , _Base32Lenient
-- , _Base32HexLenient
  -- * Patterns
, pattern Base32
, pattern Base32Unpadded
, pattern Base32Hex
, pattern Base32HexUnpadded
-- , pattern Base32Lenient
-- , pattern Base32HexLenient
) where

import Control.Lens

import Data.Text.Short (ShortText)
import qualified Data.Text.Short.Encoding.Base32 as B32TS
import qualified Data.Text.Short.Encoding.Base32.Hex as B32TSH


-- $setup
--
-- >>> import Control.Lens
-- >>> import Data.Text.Short.Encoding.Base32.Lens
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications

-- -------------------------------------------------------------------------- --
-- Optics

-- | A 'Prism' into the Base32 encoding of a 'ShortText' value.
--
-- >>> _Base32 # "Sun"
-- "KN2W4==="
--
-- >>> "KN2W4===" ^? _Base32
-- Just "Sun"
--
_Base32 :: Prism' ShortText ShortText
_Base32 = prism' B32TS.encodeBase32 $ \s -> case B32TS.decodeBase32 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base32 #-}

-- | A 'Prism' into the Base32 encoding of a 'ShortText' value.
--
-- Please note that unpadded variants should only be used
-- when assumptions about the data can be made. In particular, if the length of
-- the input is divisible by 3, then this is a safe function to call.
--
-- >>> _Base32Unpadded # "Sun"
-- "KN2W4"
--
-- >>> "KN2W4" ^? _Base32Unpadded
-- Just "Sun"
--
_Base32Unpadded :: Prism' ShortText ShortText
_Base32Unpadded = prism' B32TS.encodeBase32Unpadded $ \s -> case B32TS.decodeBase32Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base32Unpadded #-}

-- | A 'Prism' into the Base32-hex encoding of a 'ShortText' value.
--
-- >>> _Base32Hex # "Sun"
-- "ADQMS==="
--
-- >>> "ADQMS===" ^? _Base32Hex
-- Just "Sun"
--
_Base32Hex :: Prism' ShortText ShortText
_Base32Hex = prism' B32TSH.encodeBase32 $ \s -> case B32TSH.decodeBase32 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base32Hex #-}

-- | A 'Prism' into the Base32-hex encoding of a 'ShortText' value.
--
-- Please note that unpadded variants should only be used
-- when assumptions about the data can be made. In particular, if the length of
-- the input is divisible by 3, then this is a safe function to call.
--
-- >>> _Base32HexUnpadded # "Sun"
-- "ADQMS"
--
-- >>> "ADQMS" ^? _Base32HexUnpadded
-- Just "Sun"
--
_Base32HexUnpadded :: Prism' ShortText ShortText
_Base32HexUnpadded = prism' B32TSH.encodeBase32Unpadded $ \s -> case B32TSH.decodeBase32Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base32HexUnpadded #-}

-- -- | An 'Iso'' into the Base32 encoding of a 'ShortText' value
-- -- using lenient decoding.
-- --
-- --
-- -- _Note:_ This is not a lawful 'Iso'.
-- --
-- -- >>> "Sun" ^. _Base32Lenient
-- -- "U3Vu"
-- --
-- -- >>> "U3Vu" ^. from _Base32Lenient
-- -- "Sun"
-- --
-- _Base32Lenient :: Iso' ShortText ShortText
-- _Base32Lenient = iso B32TS.encodeBase32 B32TS.decodeBase32Lenient

-- -- | An 'Iso'' into the Base32hex encoding of a 'ShortText' value
-- -- using lenient decoding.
-- --
-- --
-- -- _Note:_ This is not a lawful 'Iso'.
-- --
-- -- >>> "<<??>>" ^. _Base32HexLenient
-- -- "PDw_Pz4-"
-- --
-- -- >>> "PDw_Pz4-" ^. from _Base32HexLenient
-- -- "<<??>>"
-- --
-- _Base32HexLenient :: Iso' ShortText ShortText
-- _Base32HexLenient = iso B32TSH.encodeBase32 B32TSH.decodeBase32Lenient

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Unidirectional pattern synonym for Base32-encoded 'ShortText' values.
--
pattern Base32 :: ShortText -> ShortText
pattern Base32 a <- (preview _Base32 -> Just a) where
    Base32 a = _Base32 # a

-- | Unidirectional pattern synonym for unpadded Base32-encoded 'ShortText' values.
--
pattern Base32Unpadded :: ShortText -> ShortText
pattern Base32Unpadded a <- (preview _Base32Unpadded -> Just a) where
    Base32Unpadded a = _Base32Unpadded # a

-- | Unidirectional pattern synonym for Base32hex-encoded 'ShortText' values.
--
pattern Base32Hex :: ShortText -> ShortText
pattern Base32Hex a <- (preview _Base32Hex -> Just a) where
    Base32Hex a = _Base32Hex # a

-- | Unidirectional pattern synonym for unpadded Base32hex-encoded 'ShortText' values.
--
pattern Base32HexUnpadded :: ShortText -> ShortText
pattern Base32HexUnpadded a <- (preview _Base32HexUnpadded -> Just a) where
    Base32HexUnpadded a = _Base32HexUnpadded # a

-- -- | Bidirectional pattern synonym for leniently Base32-encoded 'ShortText' values
-- --
-- pattern Base32Lenient :: ShortText -> ShortText
-- pattern Base32Lenient a <- (view (from _Base32Lenient) -> a) where
--     Base32Lenient a = view _Base32Lenient a
-- {-# COMPLETE Base32Lenient #-}

-- -- | Bidirectional pattern synonym for leniently Base32-encoded 'ShortText' values
-- --
-- pattern Base32HexLenient :: ShortText -> ShortText
-- pattern Base32HexLenient a <- (view (from _Base32HexLenient) -> a) where
--     Base32HexLenient a = view _Base32HexLenient a
-- {-# COMPLETE Base32HexLenient #-}
