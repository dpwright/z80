{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Z80.Assembler
  ( Z80
  , Z80ASM
  , ASMBlock (..)
  , org
  , code
  , db
  , defb
  , equ
  , label
  , labelled
  , withLabel
  , end
  ) where

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
  { asmOrg   :: Location
  , asmEntry :: Location
  , asmData  :: ByteString
  } deriving (Eq, Show)

code :: [Word8] -> Z80ASM
code bytes = Z80 $ do
  tell $ BS.pack bytes
  modify (+ fromIntegral (length bytes))

db :: ByteString -> Z80ASM
db bs = Z80 $ do
  tell bs
  modify (+ fromIntegral (BS.length bs))

defb :: ByteString -> Z80ASM
defb = db

label :: Z80 Location
label = Z80 get

labelled :: Z80 a -> Z80 Location
labelled asm = do
  l <- label
  asm >> return l

withLabel :: (Location -> Z80 a )-> Z80 a
withLabel asm = do
  l <- label
  asm l

end :: Z80ASM
end = return ()

org :: Location -> Z80ASM -> ASMBlock
org addr (Z80 mc) = ASMBlock { asmOrg = addr, asmEntry = addr, asmData = asm }
 where ((), _, asm) = runRWS mc () addr

equ :: a -> Z80 a
equ = return
