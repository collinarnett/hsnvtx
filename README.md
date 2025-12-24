# nvtx-hs

:warning: 100% Vibe Coded. Use at your own risk. :warning:

Haskell bindings to [NVIDIA Tools Extension (NVTX)](https://docs.nvidia.com/nsight-visual-studio-edition/nvtx/index.html) for profiling GPU applications with Nsight Systems.

## Why NVTX?

When profiling GPU applications with `nsys`, you only see low-level CUDA operations like `gemm`, `cudaMemcpy`, etc. NVTX allows you to annotate your code with named ranges and markers that show up in the profiler timeline, making it much easier to understand what your application is doing.

## Quick Start

```haskell
import NVTX

main :: IO ()
main = do
  nameThread "Main"
  
  withRange "Training" $ do
    forM_ [1..epochs] $ \epoch ->
      withRange ("Epoch " ++ show epoch) $ do
        withRange "Forward" forwardPass
        withRange "Backward" backwardPass
        withRange "Update" updateWeights
```

## Installation

### With Nix Flakes

Add to your `flake.nix`:

```nix
{
  inputs.nvtx-hs.url = "github:collinarnett/hsnvts";
  
  outputs = { self, nixpkgs, nvtx-hs, ... }:
    # Use hsnvtx.packages.${system}.hsnvtx in your build
}
```

### With Cabal

```bash
# Ensure CUDA toolkit is installed and nvToolsExt is available
cabal build
```

The package requires the `nvToolsExt` library from the CUDA toolkit. On most systems:

- **Linux**: Install CUDA toolkit, library is at `/usr/local/cuda/lib64/libnvToolsExt.so`

## API Overview

### Simple Range API (Push/Pop)

Best for nested, same-thread ranges:

```haskell
withRange :: String -> IO a -> IO a
withRangeColor :: String -> Color -> IO a -> IO a
withRangeAttribs :: EventAttribs -> IO a -> IO a
```

### Start/End Range API

For ranges that can span threads or overlap:

```haskell
startRange :: String -> IO RangeId
endRange :: RangeId -> IO ()
withStartEndRange :: String -> IO a -> IO a
```

### Markers

For instantaneous events:

```haskell
mark :: String -> IO ()
markAttribs :: EventAttribs -> IO ()
```

### Event Attributes

Full control over colors, categories, and payloads:

```haskell
data EventAttribs = EventAttribs
  { attribMessage  :: Maybe String
  , attribColor    :: Maybe Color
  , attribCategory :: Maybe CategoryId
  , attribPayload  :: Payload
  }

-- Predefined colors
colorRed, colorGreen, colorBlue, colorYellow :: Color

-- Custom colors
rgb :: Word8 -> Word8 -> Word8 -> Color
argb :: Word8 -> Word8 -> Word8 -> Word8 -> Color
```

### Resource Naming

```haskell
nameThread :: String -> IO ()
nameCategory :: CategoryId -> String -> IO ()
```

### Domain API (NVTX3)

For organizing events into separate domains:

```haskell
withDomain :: String -> (Domain -> IO a) -> IO a
withDomainRange :: Domain -> EventAttribs -> IO a -> IO a
domainMark :: Domain -> EventAttribs -> IO ()
```

## Portable Code

Use `NVTX.Safe` for code that should work with or without NVTX:

```haskell
import NVTX.Safe

main :: IO ()
main = do
  putStrLn $ "NVTX enabled: " ++ show nvtxEnabled
  withRange "MyOperation" $ do
    -- Works whether or not NVTX is available
    expensiveComputation
```

Build with NVTX disabled:

```bash
cabal build -f-nvtx
```

## Profiling with Nsight Systems

### Compile with Debug Info

For best results, compile with debug symbols so `nsys` can show Haskell function names:

```bash
ghc -g2 -rtsopts MyProgram.hs -lnvToolsExt
```

Or with Cabal:

```yaml
# cabal.project
package *
  ghc-options: -g2 -rtsopts
```

### Profile

```bash
# Basic profiling
nsys profile --trace=cuda,nvtx,osrt ./MyProgram

# With CPU sampling (shows Haskell stack traces)
nsys profile --sample=cpu --trace=cuda,nvtx,osrt ./MyProgram

# With CUDA backtraces
nsys profile --sample=cpu --trace=cuda,nvtx,osrt --cudabacktrace=all ./MyProgram

# Output to specific file
nsys profile -o myprofile --trace=cuda,nvtx,osrt ./MyProgram
```

### View Results

```bash
nsys-ui myprofile.nsys-rep
```

## Example: Profiling a Training Loop

```haskell
{-# LANGUAGE OverloadedStrings #-}

import NVTX
import Control.Monad (forM_)

data Model = Model  -- your model type
data Batch = Batch  -- your batch type

trainEpoch :: Model -> [Batch] -> IO Model
trainEpoch model batches = do
  withRange "Epoch" $ do
    foldM trainStep model (zip [1..] batches)

trainStep :: Model -> (Int, Batch) -> IO Model
trainStep model (i, batch) = do
  withRangeAttribs (defaultAttribs 
    { attribMessage = Just $ "Step " ++ show i
    , attribColor = Just colorGreen
    }) $ do
    
    -- Forward pass
    output <- withRangeColor "Forward" colorBlue $ 
      forward model batch
    
    -- Compute loss
    loss <- withRange "Loss" $ 
      computeLoss output batch
    
    -- Backward pass
    grads <- withRangeColor "Backward" colorRed $ 
      backward model loss
    
    -- Update weights
    withRangeColor "Update" colorYellow $ 
      updateWeights model grads

main :: IO ()
main = do
  nameThread "MainTrainer"
  nameCategory 1 "Training"
  nameCategory 2 "Validation"
  
  mark "Training Start"
  
  model <- initModel
  batches <- loadData
  
  finalModel <- trainEpoch model batches
  
  mark "Training Complete"
```

## Integration with Hasktorch

```haskell
import NVTX
import Torch

forward :: Tensor -> Tensor -> IO Tensor
forward weights input = withRange "Forward" $ do
  withRange "MatMul" $ do
    let hidden = matmul input weights
    withRange "Activation" $ 
      pure $ relu hidden
```

## Tips

1. **Keep ranges short**: Very short ranges (< 1μs) may not show up in the profiler or add overhead.

2. **Use colors strategically**: Color-code different types of operations (e.g., red for backward, blue for forward).

3. **Name your threads**: Call `nameThread` early in each thread for clearer profiler output.

4. **Use categories for filtering**: Assign related operations to categories, then filter in the profiler UI.

5. **Avoid in hot loops**: Don't put NVTX calls inside very tight loops; wrap the whole loop instead.

## License

BSD-3-Clause
