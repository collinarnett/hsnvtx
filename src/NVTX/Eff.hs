{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : NVTX.Eff
-- Description : Effect-based NVTX API compatible with Bluefin
--
-- This module provides an effect-based interface to NVTX that works well
-- with effect systems like Bluefin.
--
-- @
-- import NVTX.Eff
-- import Bluefin.Eff
-- import Bluefin.IO
--
-- myProgram :: IOE e -> Eff e ()
-- myProgram io = withNvtxE io $ \nvtx -> do
--   withRangeE nvtx "MyOperation" $ do
--     -- your code here
-- @

module NVTX.Eff
  ( -- * NVTX Effect
    NvtxE
  , withNvtxE
  , runNvtxIO

    -- * Range Operations  
  , withRangeE
  , withRangeColorE
  , pushRangeE
  , popRangeE

    -- * Markers
  , markE

    -- * Resource Naming
  , nameThreadE
  , nameCategoryE

    -- * Re-exports
  , Color
  , rgb
  , argb
  , CategoryId(..)
  , colorRed
  , colorGreen
  , colorBlue
  , colorYellow
  ) where

import NVTX.Types
import NVTX.Raw (c_nvtxRangePushEx)
import qualified NVTX

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CInt)

-- | The NVTX effect handle.
-- This is a simple wrapper that allows NVTX operations in effectful code.
newtype NvtxE = NvtxE ()

-- | Run NVTX operations with an effect handle.
-- This just provides a handle; NVTX functions are always available when
-- the library is linked.
withNvtxE :: MonadIO m => (NvtxE -> m a) -> m a
withNvtxE f = f (NvtxE ())

-- | Run NVTX operations directly in IO.
runNvtxIO :: (NvtxE -> IO a) -> IO a
runNvtxIO = withNvtxE

-- | Execute an action within a named range (effect-based version).
withRangeE :: MonadIO m => NvtxE -> String -> m a -> m a
withRangeE _ label action = do
  _ <- liftIO $ NVTX.pushRange label
  result <- action
  _ <- liftIO NVTX.popRange
  pure result

-- | Execute an action within a colored range (effect-based version).
withRangeColorE :: MonadIO m => NvtxE -> String -> Color -> m a -> m a
withRangeColorE _ label color action = do
  liftIO $ withEventAttribs (defaultAttribs 
    { attribMessage = Just label
    , attribColor = Just color
    }) c_nvtxRangePushEx
  result <- action
  _ <- liftIO NVTX.popRange
  pure result

-- | Push a range onto the stack (effect-based version).
pushRangeE :: MonadIO m => NvtxE -> String -> m CInt
pushRangeE _ label = liftIO $ NVTX.pushRange label

-- | Pop a range from the stack (effect-based version).
popRangeE :: MonadIO m => NvtxE -> m CInt
popRangeE _ = liftIO NVTX.popRange

-- | Create a marker (effect-based version).
markE :: MonadIO m => NvtxE -> String -> m ()
markE _ label = liftIO $ NVTX.mark label

-- | Name the current thread (effect-based version).
nameThreadE :: MonadIO m => NvtxE -> String -> m ()
nameThreadE _ name = liftIO $ NVTX.nameThread name

-- | Name a category (effect-based version).
nameCategoryE :: MonadIO m => NvtxE -> CategoryId -> String -> m ()
nameCategoryE _ cid name = liftIO $ NVTX.nameCategory cid name
