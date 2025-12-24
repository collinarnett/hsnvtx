{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : NVTX.Types
-- Description : Types for NVTX event attributes
--
-- This module defines the types used for configuring NVTX events,
-- including colors, categories, payloads, and event attributes.
module NVTX.Types
  ( -- * Event Attributes
    EventAttribs (..),
    defaultAttribs,

    -- * Colors
    Color (..),
    argb,
    rgb,
    colorRed,
    colorGreen,
    colorBlue,
    colorYellow,
    colorMagenta,
    colorCyan,
    colorWhite,
    colorOrange,

    -- * Categories
    CategoryId (..),

    -- * Payloads
    Payload (..),

    -- * Internal helpers
    withEventAttribs,
  )
where

import Data.Bits (shiftL, (.|.))
import Data.Int (Int64)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (..), poke, pokeByteOff)

-- | ARGB color for NVTX events.
-- Use 'argb' or 'rgb' smart constructors.
newtype Color = Color {unColor :: Word32}
  deriving stock (Show, Eq)
  deriving newtype (Ord)

-- | Create a color from ARGB components.
--
-- @
-- myColor = argb 255 128 64 32  -- alpha=255, red=128, green=64, blue=32
-- @
argb :: Word8 -> Word8 -> Word8 -> Word8 -> Color
argb a r g b =
  Color $
    (fromIntegral a `shiftL` 24)
      .|. (fromIntegral r `shiftL` 16)
      .|. (fromIntegral g `shiftL` 8)
      .|. fromIntegral b

-- | Create an opaque color from RGB components.
--
-- @
-- redColor = rgb 255 0 0
-- @
rgb :: Word8 -> Word8 -> Word8 -> Color
rgb = argb 255

-- | Predefined colors for convenience.
colorRed, colorGreen, colorBlue, colorYellow :: Color
colorMagenta, colorCyan, colorWhite, colorOrange :: Color
colorRed = rgb 255 0 0
colorGreen = rgb 0 255 0
colorBlue = rgb 0 0 255
colorYellow = rgb 255 255 0

colorMagenta = rgb 255 0 255

colorCyan = rgb 0 255 255

colorWhite = rgb 255 255 255

colorOrange = rgb 255 165 0

-- | Category ID for grouping NVTX events.
-- Use 'nameCategory' to associate a name with a category.
newtype CategoryId = CategoryId {unCategoryId :: Word32}
  deriving stock (Show, Eq)
  deriving newtype (Ord, Num)

-- | Payload data that can be attached to NVTX events.
data Payload
  = PayloadNone
  | PayloadUInt64 !Word64
  | PayloadInt64 !Int64
  | PayloadDouble !Double
  | PayloadUInt32 !Word32
  | PayloadInt32 !Int
  | PayloadFloat !Float
  deriving (Show, Eq)

-- | Event attributes for configuring NVTX markers and ranges.
data EventAttribs = EventAttribs
  { attribMessage :: !(Maybe String),
    attribColor :: !(Maybe Color),
    attribCategory :: !(Maybe CategoryId),
    attribPayload :: !Payload
  }
  deriving (Show, Eq)

-- | Default event attributes (all fields unset).
defaultAttribs :: EventAttribs
defaultAttribs =
  EventAttribs
    { attribMessage = Nothing,
      attribColor = Nothing,
      attribCategory = Nothing,
      attribPayload = PayloadNone
    }

-- NVTX constants (from nvToolsExt.h)
nvtx_VERSION :: CUInt
nvtx_VERSION = 3

nvtx_EVENT_ATTRIB_STRUCT_SIZE :: CUInt
nvtx_EVENT_ATTRIB_STRUCT_SIZE = 48 -- Size of nvtxEventAttributes_t

-- Color types
nvtx_COLOR_UNKNOWN :: CUInt
nvtx_COLOR_UNKNOWN = 0

nvtx_COLOR_ARGB :: CUInt
nvtx_COLOR_ARGB = 1

-- Message types
nvtx_MESSAGE_UNKNOWN :: CUInt
nvtx_MESSAGE_UNKNOWN = 0

nvtx_MESSAGE_TYPE_ASCII :: CUInt
nvtx_MESSAGE_TYPE_ASCII = 1

-- Payload types
nvtx_PAYLOAD_UNKNOWN :: CUInt
nvtx_PAYLOAD_UNKNOWN = 0

nvtx_PAYLOAD_TYPE_UNSIGNED_INT64 :: CUInt
nvtx_PAYLOAD_TYPE_UNSIGNED_INT64 = 1

nvtx_PAYLOAD_TYPE_INT64 :: CUInt
nvtx_PAYLOAD_TYPE_INT64 = 2

nvtx_PAYLOAD_TYPE_DOUBLE :: CUInt
nvtx_PAYLOAD_TYPE_DOUBLE = 3

nvtx_PAYLOAD_TYPE_UNSIGNED_INT32 :: CUInt
nvtx_PAYLOAD_TYPE_UNSIGNED_INT32 = 4

nvtx_PAYLOAD_TYPE_INT32 :: CUInt
nvtx_PAYLOAD_TYPE_INT32 = 5

nvtx_PAYLOAD_TYPE_FLOAT :: CUInt
nvtx_PAYLOAD_TYPE_FLOAT = 6

-- | Internal: Marshal EventAttribs to the C struct and call the action.
--
-- The nvtxEventAttributes_t structure layout (version 3):
--   uint16_t version;           -- offset 0
--   uint16_t size;              -- offset 2
--   uint32_t category;          -- offset 4
--   int32_t colorType;          -- offset 8
--   uint32_t color;             -- offset 12
--   int32_t payloadType;        -- offset 16
--   int32_t reserved0;          -- offset 20
--   union payload;              -- offset 24 (8 bytes)
--   int32_t messageType;        -- offset 32
--   int32_t reserved1;          -- offset 36
--   union message;              -- offset 40 (8 bytes = pointer)
--   total size: 48 bytes
withEventAttribs :: EventAttribs -> (Ptr () -> IO a) -> IO a
withEventAttribs attribs action =
  alloca $ \ptr -> do
    -- Zero the structure first
    fillBytes ptr 0 48

    -- Set version and size
    pokeByteOff ptr 0 (fromIntegral nvtx_VERSION :: Word16)
    pokeByteOff ptr 2 (fromIntegral nvtx_EVENT_ATTRIB_STRUCT_SIZE :: Word16)

    -- Set category
    case attribCategory attribs of
      Nothing -> pokeByteOff ptr 4 (0 :: Word32)
      Just (CategoryId c) -> pokeByteOff ptr 4 c

    -- Set color
    case attribColor attribs of
      Nothing -> do
        pokeByteOff ptr 8 (fromIntegral nvtx_COLOR_UNKNOWN :: CInt)
        pokeByteOff ptr 12 (0 :: Word32)
      Just (Color c) -> do
        pokeByteOff ptr 8 (fromIntegral nvtx_COLOR_ARGB :: CInt)
        pokeByteOff ptr 12 c

    -- Set payload
    case attribPayload attribs of
      PayloadNone -> do
        pokeByteOff ptr 16 (fromIntegral nvtx_PAYLOAD_UNKNOWN :: CInt)
      PayloadUInt64 v -> do
        pokeByteOff ptr 16 (fromIntegral nvtx_PAYLOAD_TYPE_UNSIGNED_INT64 :: CInt)
        pokeByteOff ptr 24 v
      PayloadInt64 v -> do
        pokeByteOff ptr 16 (fromIntegral nvtx_PAYLOAD_TYPE_INT64 :: CInt)
        pokeByteOff ptr 24 v
      PayloadDouble v -> do
        pokeByteOff ptr 16 (fromIntegral nvtx_PAYLOAD_TYPE_DOUBLE :: CInt)
        pokeByteOff ptr 24 v
      PayloadUInt32 v -> do
        pokeByteOff ptr 16 (fromIntegral nvtx_PAYLOAD_TYPE_UNSIGNED_INT32 :: CInt)
        pokeByteOff ptr 24 v
      PayloadInt32 v -> do
        pokeByteOff ptr 16 (fromIntegral nvtx_PAYLOAD_TYPE_INT32 :: CInt)
        pokeByteOff ptr 24 (fromIntegral v :: Word32)
      PayloadFloat v -> do
        pokeByteOff ptr 16 (fromIntegral nvtx_PAYLOAD_TYPE_FLOAT :: CInt)
        pokeByteOff ptr 24 v

    -- Set message
    case attribMessage attribs of
      Nothing -> do
        pokeByteOff ptr 32 (fromIntegral nvtx_MESSAGE_UNKNOWN :: CInt)
        pokeByteOff ptr 40 nullPtr
        action ptr
      Just msg -> withCString msg $ \cstr -> do
        pokeByteOff ptr 32 (fromIntegral nvtx_MESSAGE_TYPE_ASCII :: CInt)
        pokeByteOff ptr 40 cstr
        action ptr
