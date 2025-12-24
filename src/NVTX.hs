{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : NVTX
-- Description : High-level Haskell bindings to NVIDIA Tools Extension (NVTX)
--
-- This module provides safe, idiomatic Haskell bindings to NVTX for annotating
-- code regions in GPU profilers like nsys (Nsight Systems).
--
-- = Basic Usage
--
-- @
-- import NVTX
--
-- main :: IO ()
-- main = do
--   withRange "Main" $ do
--     withRange "Forward Pass" $ do
--       forwardPass
--     withRange "Backward Pass" $ do
--       backwardPass
-- @
--
-- = Profiling with nsys
--
-- Compile with debugging info for better stack traces:
--
-- @
-- ghc -g2 -rtsopts MyProgram.hs -lnvToolsExt
-- @
--
-- Profile with:
--
-- @
-- nsys profile --sample=cpu --trace=cuda,nvtx,osrt --cudabacktrace=all ./MyProgram
-- @

module NVTX
  ( -- * Simple Range API (Push/Pop)
    withRange
  , withRangeColor
  , withRangeAttribs
  , pushRange
  , popRange

    -- * Start/End Range API (Non-nested, Cross-thread)
  , RangeId
  , startRange
  , startRangeAttribs
  , endRange
  , withStartEndRange

    -- * Markers (Instantaneous Events)
  , mark
  , markAttribs

    -- * Event Attributes
  , EventAttribs(..)
  , defaultAttribs
  , Color(..)
  , argb
  , rgb
  , CategoryId(..)
  , Payload(..)

    -- * Resource Naming
  , nameThread
  , nameCategory

    -- * Domain API (NVTX3)
  , Domain
  , createDomain
  , destroyDomain
  , withDomain
  , domainRangePush
  , domainRangePop
  , withDomainRange
  , domainMark
  ) where

import NVTX.Raw
import NVTX.Types

import Control.Exception (bracket, bracket_, finally, mask_)
import Control.Monad (void)
import Data.Word (Word32, Word64)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)

-- | Execute an action within a named NVTX range using push/pop semantics.
-- The range will be properly closed even if an exception is thrown.
--
-- @
-- withRange "MyOperation" $ do
--   someExpensiveComputation
-- @
withRange :: String -> IO a -> IO a
withRange label action =
  bracket_ (pushRange label) popRange action

-- | Execute an action within a colored NVTX range.
--
-- @
-- withRangeColor "Forward" (rgb 255 0 0) $ do
--   forwardPass
-- @
withRangeColor :: String -> Color -> IO a -> IO a
withRangeColor label color action =
  withRangeAttribs (defaultAttribs { attribMessage = Just label, attribColor = Just color }) action

-- | Execute an action within an NVTX range with full attribute control.
withRangeAttribs :: EventAttribs -> IO a -> IO a
withRangeAttribs attribs action =
  bracket_ (pushRangeAttribs attribs) popRange action

-- | Push a named range onto the thread's range stack.
-- Must be paired with 'popRange'.
pushRange :: String -> IO CInt
pushRange label = withCString label c_nvtxRangePushA

-- | Push a range with attributes onto the thread's range stack.
pushRangeAttribs :: EventAttribs -> IO CInt
pushRangeAttribs attribs = withEventAttribs attribs c_nvtxRangePushEx

-- | Pop the most recent range from the thread's range stack.
popRange :: IO CInt
popRange = c_nvtxRangePop

-- | A unique identifier for a start/end range.
-- Unlike push/pop ranges, start/end ranges can span threads.
newtype RangeId = RangeId { unRangeId :: Word64 }
  deriving stock (Show, Eq)
  deriving newtype (Ord)

-- | Start a named range that can span threads.
-- Returns a 'RangeId' that must be passed to 'endRange'.
startRange :: String -> IO RangeId
startRange label = RangeId <$> withCString label c_nvtxRangeStartA

-- | Start a range with attributes.
startRangeAttribs :: EventAttribs -> IO RangeId
startRangeAttribs attribs = RangeId <$> withEventAttribs attribs c_nvtxRangeStartEx

-- | End a range started with 'startRange' or 'startRangeAttribs'.
endRange :: RangeId -> IO ()
endRange (RangeId rid) = c_nvtxRangeEnd rid

-- | Execute an action within a start/end range.
-- Unlike 'withRange', the start and end can theoretically be on different threads,
-- though in this wrapper they're on the same thread.
withStartEndRange :: String -> IO a -> IO a
withStartEndRange label action =
  bracket (startRange label) endRange (const action)

-- | Create an instantaneous marker event with a message.
mark :: String -> IO ()
mark label = withCString label c_nvtxMarkA

-- | Create a marker with full attributes.
markAttribs :: EventAttribs -> IO ()
markAttribs attribs = withEventAttribs attribs c_nvtxMarkEx

-- | Name the current OS thread for display in profilers.
--
-- @
-- nameThread "Main Worker"
-- @
nameThread :: String -> IO ()
nameThread name = do
  tid <- getCurrentThreadId
  withCString name $ \cname -> c_nvtxNameOsThreadA tid cname

-- | Name a category for grouping events.
nameCategory :: CategoryId -> String -> IO ()
nameCategory (CategoryId cid) name =
  withCString name $ \cname -> c_nvtxNameCategoryA cid cname

-- | Create a named domain for organizing NVTX events (NVTX3).
-- Returns Nothing if NVTX3 is not available or domain creation fails.
createDomain :: String -> IO (Maybe Domain)
createDomain name = do
  d <- withCString name c_nvtxDomainCreateA
  if d == nullDomain
    then return Nothing
    else return (Just d)

-- | Destroy a domain created with 'createDomain'.
destroyDomain :: Domain -> IO ()
destroyDomain = c_nvtxDomainDestroy

-- | Execute an action with a domain, destroying it afterwards.
withDomain :: String -> (Domain -> IO a) -> IO a
withDomain name action = bracket
  (createDomain name >>= maybe (error $ "Failed to create domain: " ++ name) return)
  destroyDomain
  action

-- | Push a range in a specific domain.
domainRangePush :: Domain -> EventAttribs -> IO CInt
domainRangePush domain attribs = 
  withEventAttribs attribs $ \ptr -> c_nvtxDomainRangePushEx domain ptr

-- | Pop a range in a specific domain.
domainRangePop :: Domain -> IO CInt
domainRangePop = c_nvtxDomainRangePop

-- | Execute an action within a domain range.
withDomainRange :: Domain -> EventAttribs -> IO a -> IO a
withDomainRange domain attribs action =
  bracket_ (domainRangePush domain attribs) (domainRangePop domain) action

-- | Create a marker in a specific domain.
domainMark :: Domain -> EventAttribs -> IO ()
domainMark domain attribs = withEventAttribs attribs $ \ptr ->
  c_nvtxDomainMarkEx domain ptr
