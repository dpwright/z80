{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Z80.Assembler
  ( Z80
  , Z80ASM
  , ASMBlock (..)
  , org
  , code
  , db
  , equ
  , label ) where

import Data.Word
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

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

code :: [Word8] -> Z80ASM
code bytes = Z80 $ do
  tell $ BS.pack bytes
  modify (+ fromIntegral (length bytes))

db :: ByteString -> Z80ASM
db bs = Z80 $ do
  tell bs
  modify (+ fromIntegral (BS.length bs))

label :: Z80 Location
label = Z80 get

org :: Location -> Z80ASM -> ASMBlock
org addr (Z80 mc) = ASMBlock { asmOrg = addr, asmData = asm }
 where ((), _, asm) = runRWS mc () addr

equ :: a -> Z80 a
equ = return
