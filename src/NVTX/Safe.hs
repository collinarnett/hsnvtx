{-# LANGUAGE CPP #-}

-- |
-- Module      : NVTX.Safe
-- Description : Safe NVTX bindings that gracefully handle missing library
--
-- This module provides NVTX bindings that do nothing when NVTX is unavailable.
-- Useful for portable code that may run on systems without NVIDIA tooling.
--
-- When compiled with the @nvtx@ flag disabled, all functions become no-ops.
-- When compiled with the @nvtx@ flag enabled, they delegate to "NVTX".
--
-- Usage in cabal:
-- @
-- flag nvtx
--   description: Enable NVTX profiling support
--   default: True
--   manual: True
--
-- library
--   if flag(nvtx)
--     cpp-options: -DNVTX_ENABLED
--     extra-libraries: nvToolsExt
-- @

module NVTX.Safe
  ( -- * Range API
    withRange
  , withRangeColor
  , pushRange
  , popRange

    -- * Markers
  , mark

    -- * Resource Naming
  , nameThread

    -- * Compile-time check
  , nvtxEnabled
  ) where

#ifdef NVTX_ENABLED
import qualified NVTX
import NVTX.Types (Color)
import Foreign.C.Types (CInt)
#endif

-- | Whether NVTX support is compiled in.
nvtxEnabled :: Bool
#ifdef NVTX_ENABLED
nvtxEnabled = True
#else
nvtxEnabled = False
#endif

-- | Execute an action within a named range (no-op if NVTX disabled).
withRange :: String -> IO a -> IO a
#ifdef NVTX_ENABLED
withRange = NVTX.withRange
#else
withRange _ action = action
#endif

-- | Execute an action within a colored range (no-op if NVTX disabled).
#ifdef NVTX_ENABLED
withRangeColor :: String -> Color -> IO a -> IO a
withRangeColor = NVTX.withRangeColor
#else
withRangeColor :: String -> a -> IO b -> IO b
withRangeColor _ _ action = action
#endif

-- | Push a named range (no-op returning -1 if NVTX disabled).
#ifdef NVTX_ENABLED
pushRange :: String -> IO CInt
pushRange = NVTX.pushRange
#else
pushRange :: String -> IO Int
pushRange _ = return (-1)
#endif

-- | Pop the current range (no-op returning -1 if NVTX disabled).
#ifdef NVTX_ENABLED
popRange :: IO CInt
popRange = NVTX.popRange
#else
popRange :: IO Int
popRange = return (-1)
#endif

-- | Create a marker (no-op if NVTX disabled).
mark :: String -> IO ()
#ifdef NVTX_ENABLED
mark = NVTX.mark
#else
mark _ = return ()
#endif

-- | Name the current thread (no-op if NVTX disabled).
nameThread :: String -> IO ()
#ifdef NVTX_ENABLED
nameThread = NVTX.nameThread
#else
nameThread _ = return ()
#endif
