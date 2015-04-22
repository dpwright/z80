module Z80.Operands
  ( -- * Generic Registers
    Reg8 (..)
  , Reg16 (..)
    -- * Special Registers
  , A (..), F (..), I (..), R (..)
  , HL (..), AF (..), SP (..), PC (..)
    -- * Index Registers & Offsets
  , RegIx (..)
  , IxOffset (..)
    -- * Addresses
  , Location
  ) where

import Data.Word

data Reg8  = B | C | D | E | H | L deriving (Eq, Show)
data Reg16 = BC | DE deriving (Eq, Show)
data RegIx = IX | IY deriving (Eq, Show)

-- Separating A from the other 8-bit registers allows me to define
-- certain operations that *only* work with the accumulator, but at
-- the cost of making me repeat code for standard Reg8 operands.
data A  = A deriving (Eq, Show)
data F  = F deriving (Eq, Show)
data I  = I deriving (Eq, Show)
data R  = R deriving (Eq, Show)
data HL = HL deriving (Eq, Show)
data AF = AF deriving (Eq, Show)
data SP = SP deriving (Eq, Show)
data PC = PC deriving (Eq, Show)

data IxOffset = RegIx :+ Word8 deriving (Eq, Show)

type Location = Word16
