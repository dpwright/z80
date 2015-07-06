-- | Macros which should be useful when working with any Z80-based system.

module Z80.Macros
  ( ldVia
  , decLoopB
  ) where

import Z80.Assembler

import Z80.Operands
import Z80.Operations

import Data.Word

-- | ldVia (load via) lets you load a value that you couldn't usually load directly
-- by using an intermediate register/memory location.
ldVia :: (Load a c, Load b a)
      => a      -- ^ Intermediate register/location
      -> b      -- ^ Destination register/location
      -> c      -- ^ Source register/location
      -> Z80ASM
ldVia intermediate destination source = do
  ld intermediate source
  ld destination intermediate

-- | decLoopB runs the supplied assembly `count` times, filling the B register with
-- count and then decrementing until it reaches 0.
decLoopB :: Word8  -- ^ Number of iterations to run
         -> Z80ASM -- ^ Code to run each iteration
         -> Z80ASM
decLoopB count asm = do
  ld B count
  withLabel $ \loop -> asm >> djnz loop
