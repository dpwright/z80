{-# LANGUAGE TypeOperators #-}

module Z80.Operands
  ( -- * Generic Registers
    Reg8 (..)
  , Reg16
  , Encodable (..)
    -- * Special Registers
  , A (..), F (..), I (..), R (..)
  , BC (..), DE (..), HL (..)
  , AF (..), SP (..), PC (..)
    -- * Shadow Registers
  , AF' (..)
    -- * Index Registers & Offsets
  , RegIx (..)
    -- * Addresses
  , Location
  ) where

import Data.Word

data Reg8  = B | C | D | E | H | L deriving (Eq, Show)

-- Separating A from the other 8-bit registers allows me to define
-- certain operations that *only* work with the accumulator, but at
-- the cost of making me repeat code for standard Reg8 operands.
data A  = A deriving (Eq, Show)
data F  = F deriving (Eq, Show)
data I  = I deriving (Eq, Show)
data R  = R deriving (Eq, Show)
data BC = BC deriving (Eq, Show)
data DE = DE deriving (Eq, Show)
data HL = HL deriving (Eq, Show)
data AF = AF deriving (Eq, Show)
data SP = SP deriving (Eq, Show)
data PC = PC deriving (Eq, Show)

data AF' = AF' deriving (Eq, Show)

infixl 4 :+
data RegIx = IX | IY | Maybe RegIx :+ Word8 deriving (Eq, Show)

-- Extremely dodgy Num instance for ease-of-use.
-- For now just applying all operations to the offset component, but there'd
-- be a good argument for making certain operations (negate, etc) errors...
instance Num RegIx where
  (+)             = ixBinOp (+)
  (*)             = ixBinOp (*)
  (-)             = ixBinOp (-)
  negate (r :+ d) = r :+ negate d
  negate i        = error $ "negate " ++ show i ++ " does not make sense!"
  abs (r :+ d)    = r :+ abs d
  abs i           = error $ "abs " ++ show i ++ " does not make sense!"
  signum (r :+ d) = r :+ signum d
  signum i        = error $ "signum " ++ show i ++ " does not make sense!"
  fromInteger i   = Nothing :+ fromInteger i

reduceRegIx :: RegIx -> RegIx
reduceRegIx (Just (r :+ d) :+ d') = reduceRegIx (r :+ d + d')
reduceRegIx r                     = r

ixBinOp :: (Word8 -> Word8 -> Word8) -> RegIx -> RegIx -> RegIx
ixBinOp op ix ix' = go (reduceRegIx ix) (reduceRegIx ix') where
  go (r :+ d) (r' :+ d') = compatibleIx r r'        :+ op d d'
  go i        (r' :+ d') = compatibleIx (Just i) r' :+ op 0 d'
  go (r :+ d) i          = compatibleIx r (Just i)  :+ op d 0
  go i        i'         = error $ "Binary numerical operations on " ++ show i ++ " and " ++ show i' ++ " do not make sense!"

compatibleIx :: Maybe RegIx -> Maybe RegIx -> Maybe RegIx
compatibleIx i Nothing = i
compatibleIx Nothing i = i
compatibleIx (Just i) (Just i') =
  error $ "Cannot add two indices: " ++ show i ++ " and " ++ show i'

type Location = Word16

class Encodable r where
  encode :: r -> Word8

instance Encodable A where
  encode A = 0x7 -- 111

instance Encodable Reg8 where
  encode B = 0x0 -- 000
  encode C = 0x1 -- 001
  encode D = 0x2 -- 010
  encode E = 0x3 -- 011
  encode H = 0x4 -- 100
  encode L = 0x5 -- 101

instance Encodable BC where
  encode BC = 0x0 -- 00
instance Encodable DE where
  encode DE = 0x1 -- 01
instance Encodable HL where
  encode HL = 0x2 -- 10

-- SP and AF both encode to the same value.
-- This will break if there is ever a situation in which either could be passed.
instance Encodable SP where
  encode SP = 0x3 -- 11
instance Encodable AF where
  encode AF = 0x3 -- 11

class Encodable r => Reg16 r
instance Reg16 BC
instance Reg16 DE
