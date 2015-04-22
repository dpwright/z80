{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Z80.Assembler
  ( Z80
  , Z80ASM
  , ASMBlock (..)
  , org
  , db
  , label ) where

import Data.ByteString as BS

import Control.Monad.RWS

import Control.Applicative
import Prelude

import Z80.Operands

newtype Z80 a = Z80 (RWS () ByteString Location a)
  deriving (Functor, Applicative, Monad, MonadFix)
type Z80ASM = Z80 ()

data ASMBlock
  = ASMBlock
  { asmOrg  :: Location
  , asmData :: ByteString
  } deriving (Eq, Show)

db :: ByteString -> Z80ASM
db bs = Z80 $ do
  tell bs
  modify (+ fromIntegral (BS.length bs))

label :: Z80 Location
label = Z80 get

org :: Location -> Z80ASM -> ASMBlock
org addr (Z80 mc) = ASMBlock { asmOrg = addr, asmData = asm }
 where ((), _, asm) = runRWS mc () addr
