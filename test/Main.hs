{-# LANGUAGE CPP #-}

module Main where

#ifdef NVTX_ENABLED
import NVTX
import NVTX.Types
#endif

import NVTX.Safe qualified as Safe
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  putStrLn $ "NVTX enabled: " ++ show Safe.nvtxEnabled
  
#ifdef NVTX_ENABLED
  -- Full API tests
  testFullAPI
#endif
  
  -- Safe API tests (work regardless of NVTX availability)
  testSafeAPI
  
  putStrLn "All tests passed!"

testSafeAPI :: IO ()
testSafeAPI = do
  putStrLn "Testing Safe API..."
  
  Safe.nameThread "TestThread"
  
  Safe.withRange "SafeTest" $ do
    Safe.mark "SafeMarker"
    threadDelay 1000  -- 1ms
  
  putStrLn "Safe API tests passed."

#ifdef NVTX_ENABLED
testFullAPI :: IO ()
testFullAPI = do
  putStrLn "Testing Full API..."
  
  -- Thread naming
  nameThread "MainTestThread"
  
  -- Category naming
  nameCategory 1 "TestCategory"
  
  -- Simple push/pop
  _ <- pushRange "ManualRange"
  threadDelay 1000
  _ <- popRange
  
  -- withRange
  withRange "AutoRange" $ do
    threadDelay 1000
  
  -- Colored ranges
  withRangeColor "ColoredRange" colorRed $ do
    threadDelay 1000
  
  -- Full attributes
  withRangeAttribs (defaultAttribs
    { attribMessage = Just "FullAttribs"
    , attribColor = Just (rgb 128 64 255)
    , attribCategory = Just 1
    , attribPayload = PayloadInt64 42
    }) $ do
    threadDelay 1000
  
  -- Nested ranges
  withRange "Outer" $ do
    withRange "Middle" $ do
      withRange "Inner" $ do
        mark "DeepMarker"
        threadDelay 500
  
  -- Start/End ranges
  rid <- startRange "StartEndRange"
  threadDelay 1000
  endRange rid
  
  -- Markers
  mark "SimpleMarker"
  
  markAttribs (defaultAttribs
    { attribMessage = Just "AttributedMarker"
    , attribColor = Just colorGreen
    })
  
  putStrLn "Full API tests passed."
#endif
