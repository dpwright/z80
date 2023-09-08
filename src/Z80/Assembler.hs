{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Z80.Assembler
  ( Z80
  , Z80ASM
  , ASMBlock (..)
  , org
  , code
  , Bytes (..)
  , db
  , resb
  , equ
  , label
  , labelled
  , withLabel
  , end
  , beginExecution
  ) where

import Data.Word
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Control.Monad.RWS
import Data.Maybe

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Traversable (traverse)
#endif
import Prelude

import Z80.Operands

data ASMState
  = ASMState
  { loc   :: Location
  , lastLoc :: Location
  , entry :: Maybe Location
  }

newtype Z80 a = Z80 (RWS () ByteString ASMState a)
  deriving (Functor, Applicative, Monad, MonadFix)
type Z80ASM = Z80 ()

data ASMBlock
  = ASMBlock
  { asmOrg   :: Location
  , asmEntry :: Location
  , asmData  :: ByteString
  } deriving (Eq, Show)

fillToLoc :: Z80ASM
fillToLoc = Z80 $ do
    padding <- gets $ \st -> loc st - lastLoc st
    tell $ BS.replicate (fromIntegral padding) 0x00

incrementLoc :: Location -> Z80ASM
incrementLoc x = do
    fillToLoc
    Z80 $ modify $ \st -> let loc' = loc st + x in st{ loc = loc', lastLoc = loc' }

reserveLoc :: Location -> ASMState -> ASMState
reserveLoc x st = st{ loc = loc st + x }

tellBytes :: [Word8] -> Z80ASM
tellBytes bytes = do
    Z80 $ tell $ BS.pack bytes
    incrementLoc . fromIntegral $ length bytes
      -- The new location has to be computed lazily in the actual
      -- content of the bytes, so that we can emit byte values
      -- referring to later labels.

code :: [Word8] -> Z80ASM
code = tellBytes

class Bytes a where
  defb :: a -> Z80ASM

instance Bytes ByteString where
  defb = defByteString
instance (b ~ Word8) => Bytes [b] where
  defb = tellBytes

db :: Bytes a => a -> Z80ASM
db = defb

resb :: Word16 -> Z80ASM
resb n = Z80 $ do
    modify $ reserveLoc n

defByteString :: ByteString -> Z80ASM
defByteString bs = do
  Z80 $ tell bs
  incrementLoc . fromIntegral $ BS.length bs

label :: Z80 Location
label = loc <$> Z80 get

labelled :: Z80 a -> Z80 Location
labelled asm = do
  l <- label
  asm >> return l

withLabel :: (Location -> Z80 a) -> Z80 a
withLabel asm = do
  l <- label
  asm l

end :: Z80ASM
end = return ()

beginExecution :: Z80ASM
beginExecution = do
  l <- label
  Z80 . modify $ setEntry l
  where setEntry l st = case entry st of
            Nothing -> st{ entry = Just l }
            Just e ->
              error $ "Cannot set execution start point twice.  First start point: " ++ show e ++
                        " This start point: " ++ show l

org :: Location -> Z80ASM -> ASMBlock
org addr (Z80 mc) = ASMBlock { asmOrg = addr,
                               asmEntry = fromMaybe addr $ entry finalState,
                               asmData = truncate asm }
 where ((), finalState, asm) = runRWS mc () (ASMState addr addr Nothing)
       truncate = BS.take (fromIntegral $ lastLoc finalState - addr)

equ :: a -> Z80 a
equ = return
