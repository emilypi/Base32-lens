{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Lazy.Base32.Lens
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: non-portable
--
-- This module contains 'Prism''s for Base32-encoding and
-- decoding 'ByteString' values.
--
module Data.ByteString.Lazy.Base32.Lens
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

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Base32 as LB32
import qualified Data.ByteString.Lazy.Base32.Hex as LB32H


-- $setup
--
-- >>> import Control.Lens
-- >>> import Data.ByteString.Lazy.Base32.Lens
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications


-- -------------------------------------------------------------------------- --
-- Optics

-- | A 'Prism'' into the Base32 encoding of a 'ByteString' value
--
-- >>> _Base32 # "Sun"
-- "KN2W4==="
--
-- >>> "KN2W4===" ^? _Base32
-- Just "Sun"
--
_Base32 :: Prism' ByteString ByteString
_Base32 = prism' LB32.encodeBase32' $ \s -> case LB32.decodeBase32 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base32 #-}

-- | A 'Prism'' into the Base32 encoding of a 'ByteString' value
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
_Base32Unpadded :: Prism' ByteString ByteString
_Base32Unpadded = prism' LB32.encodeBase32Unpadded' $ \s -> case LB32.decodeBase32Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base32Unpadded #-}

-- | A 'Prism'' into the Base32hex encoding of a 'ByteString' value
--
-- >>> _Base32Hex # "Sun"
-- "ADQMS==="
--
-- >>> "ADQMS===" ^? _Base32Hex
-- Just "Sun"
--
_Base32Hex :: Prism' ByteString ByteString
_Base32Hex = prism' LB32H.encodeBase32' $ \s -> case LB32H.decodeBase32 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base32Hex #-}

-- | A 'Prism'' into the Base32hex encoding of a 'ByteString' value
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
_Base32HexUnpadded :: Prism' ByteString ByteString
_Base32HexUnpadded = prism' LB32H.encodeBase32Unpadded' $ \s -> case LB32H.decodeBase32Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base32HexUnpadded #-}

-- -- | An 'Iso'' into the Base32 encoding of a 'ByteString' value
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
-- _Base32Lenient :: Iso' ByteString ByteString
-- _Base32Lenient = iso LB32.encodeBase32' LB32.decodeBase32Lenient

-- -- | An 'Iso'' into the Base32hex encoding of a 'ByteString' value
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
-- _Base32HexLenient :: Iso' ByteString ByteString
-- _Base32HexLenient = iso LB32H.encodeBase32' LB32H.decodeBase32Lenient

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Bidirectional pattern synonym for Base32-encoded 'ByteString' values.
--
pattern Base32 :: ByteString -> ByteString
pattern Base32 a <- (preview _Base32 -> Just a) where
    Base32 a = _Base32 # a

-- | Bidirectional pattern synonym for unpadded Base32-encoded 'ByteString' values.
--
pattern Base32Unpadded :: ByteString -> ByteString
pattern Base32Unpadded a <- (preview _Base32Unpadded -> Just a) where
    Base32Unpadded a = _Base32Unpadded # a

-- | Bidirectional pattern synonym for Base32hex-encoded 'ByteString' values.
--
pattern Base32Hex :: ByteString -> ByteString
pattern Base32Hex a <- (preview _Base32Hex -> Just a) where
    Base32Hex a = _Base32Hex # a

-- | Bidirectional pattern synonym for unpadded Base32hex-encoded 'ByteString' values.
--
pattern Base32HexUnpadded :: ByteString -> ByteString
pattern Base32HexUnpadded a <- (preview _Base32HexUnpadded -> Just a) where
    Base32HexUnpadded a = _Base32HexUnpadded # a

-- -- | Bidirectional pattern synonym for leniently Base32-encoded 'ByteString' values
-- --
-- pattern Base32Lenient :: ByteString -> ByteString
-- pattern Base32Lenient a <- (view (from _Base32Lenient) -> a) where
--     Base32Lenient a = view _Base32Lenient a
-- {-# COMPLETE Base32Lenient #-}

-- -- | Bidirectional pattern synonym for leniently Base32-encoded 'ByteString' values
-- --
-- pattern Base32HexLenient :: ByteString -> ByteString
-- pattern Base32HexLenient a <- (view (from _Base32HexLenient) -> a) where
--     Base32HexLenient a = view _Base32HexLenient a
-- {-# COMPLETE Base32HexLenient #-}
