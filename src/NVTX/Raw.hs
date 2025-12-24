{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : NVTX.Raw
-- Description : Raw FFI bindings to NVTX C library
--
-- Low-level bindings to the NVTX C API. Most users should use the
-- high-level API in "NVTX" instead.
--
-- Link with @-lnvToolsExt@ or include the NVTX3 headers (header-only in v3).

module NVTX.Raw
  ( -- * Markers
    c_nvtxMarkA
  , c_nvtxMarkEx

    -- * Push/Pop Ranges
  , c_nvtxRangePushA
  , c_nvtxRangePushEx
  , c_nvtxRangePop

    -- * Start/End Ranges
  , c_nvtxRangeStartA
  , c_nvtxRangeStartEx
  , c_nvtxRangeEnd

    -- * Resource Naming
  , c_nvtxNameOsThreadA
  , c_nvtxNameCategoryA

    -- * Domain API (NVTX3)
  , Domain
  , nullDomain
  , c_nvtxDomainCreateA
  , c_nvtxDomainDestroy
  , c_nvtxDomainRangePushEx
  , c_nvtxDomainRangePop
  , c_nvtxDomainMarkEx

    -- * Thread ID
  , getCurrentThreadId
  ) where

import Data.Word (Word32, Word64)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CULong(..))
import Foreign.Ptr (Ptr, nullPtr)

-- | Get current thread ID via pthread_self
foreign import ccall unsafe "pthread.h pthread_self"
  c_pthread_self :: IO CULong

getCurrentThreadId :: IO Word32
getCurrentThreadId = fromIntegral <$> c_pthread_self

-- | Opaque domain handle for NVTX3 domain API.
newtype Domain = Domain (Ptr ())
  deriving (Show, Eq)

-- | Null domain handle.
nullDomain :: Domain
nullDomain = Domain nullPtr

-- Markers

-- | Create an ASCII marker.
-- void nvtxMarkA(const char* message);
foreign import ccall unsafe "nvToolsExt.h nvtxMarkA"
  c_nvtxMarkA :: CString -> IO ()

-- | Create a marker with attributes.
-- void nvtxMarkEx(const nvtxEventAttributes_t* eventAttrib);
foreign import ccall unsafe "nvToolsExt.h nvtxMarkEx"
  c_nvtxMarkEx :: Ptr () -> IO ()

-- Push/Pop Ranges

-- | Push an ASCII range.
-- int nvtxRangePushA(const char* message);
foreign import ccall unsafe "nvToolsExt.h nvtxRangePushA"
  c_nvtxRangePushA :: CString -> IO CInt

-- | Push a range with attributes.
-- int nvtxRangePushEx(const nvtxEventAttributes_t* eventAttrib);
foreign import ccall unsafe "nvToolsExt.h nvtxRangePushEx"
  c_nvtxRangePushEx :: Ptr () -> IO CInt

-- | Pop the current range.
-- int nvtxRangePop(void);
foreign import ccall unsafe "nvToolsExt.h nvtxRangePop"
  c_nvtxRangePop :: IO CInt

-- Start/End Ranges

-- | Start an ASCII range.
-- nvtxRangeId_t nvtxRangeStartA(const char* message);
foreign import ccall unsafe "nvToolsExt.h nvtxRangeStartA"
  c_nvtxRangeStartA :: CString -> IO Word64

-- | Start a range with attributes.
-- nvtxRangeId_t nvtxRangeStartEx(const nvtxEventAttributes_t* eventAttrib);
foreign import ccall unsafe "nvToolsExt.h nvtxRangeStartEx"
  c_nvtxRangeStartEx :: Ptr () -> IO Word64

-- | End a range.
-- void nvtxRangeEnd(nvtxRangeId_t id);
foreign import ccall unsafe "nvToolsExt.h nvtxRangeEnd"
  c_nvtxRangeEnd :: Word64 -> IO ()

-- Resource Naming

-- | Name an OS thread.
-- void nvtxNameOsThreadA(uint32_t threadId, const char* name);
foreign import ccall unsafe "nvToolsExt.h nvtxNameOsThreadA"
  c_nvtxNameOsThreadA :: Word32 -> CString -> IO ()

-- | Name a category.
-- void nvtxNameCategoryA(uint32_t category, const char* name);
foreign import ccall unsafe "nvToolsExt.h nvtxNameCategoryA"
  c_nvtxNameCategoryA :: Word32 -> CString -> IO ()

-- Domain API (NVTX3)

-- | Create a domain.
-- nvtxDomainHandle_t nvtxDomainCreateA(const char* name);
foreign import ccall unsafe "nvToolsExt.h nvtxDomainCreateA"
  c_nvtxDomainCreateA_raw :: CString -> IO (Ptr ())

c_nvtxDomainCreateA :: CString -> IO Domain
c_nvtxDomainCreateA name = Domain <$> c_nvtxDomainCreateA_raw name

-- | Destroy a domain.
-- void nvtxDomainDestroy(nvtxDomainHandle_t domain);
foreign import ccall unsafe "nvToolsExt.h nvtxDomainDestroy"
  c_nvtxDomainDestroy_raw :: Ptr () -> IO ()

c_nvtxDomainDestroy :: Domain -> IO ()
c_nvtxDomainDestroy (Domain p) = c_nvtxDomainDestroy_raw p

-- | Push a range in a domain.
-- int nvtxDomainRangePushEx(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
foreign import ccall unsafe "nvToolsExt.h nvtxDomainRangePushEx"
  c_nvtxDomainRangePushEx_raw :: Ptr () -> Ptr () -> IO CInt

c_nvtxDomainRangePushEx :: Domain -> Ptr () -> IO CInt
c_nvtxDomainRangePushEx (Domain d) attrib = c_nvtxDomainRangePushEx_raw d attrib

-- | Pop a range in a domain.
-- int nvtxDomainRangePop(nvtxDomainHandle_t domain);
foreign import ccall unsafe "nvToolsExt.h nvtxDomainRangePop"
  c_nvtxDomainRangePop_raw :: Ptr () -> IO CInt

c_nvtxDomainRangePop :: Domain -> IO CInt
c_nvtxDomainRangePop (Domain d) = c_nvtxDomainRangePop_raw d

-- | Create a marker in a domain.
-- void nvtxDomainMarkEx(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
foreign import ccall unsafe "nvToolsExt.h nvtxDomainMarkEx"
  c_nvtxDomainMarkEx_raw :: Ptr () -> Ptr () -> IO ()

c_nvtxDomainMarkEx :: Domain -> Ptr () -> IO ()
c_nvtxDomainMarkEx (Domain d) attrib = c_nvtxDomainMarkEx_raw d attrib
