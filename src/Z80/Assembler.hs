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

incrementLoc :: Location -> ASMState -> ASMState
incrementLoc x st = st { loc = loc st + x }

code :: [Word8] -> Z80ASM
code bytes = Z80 $ do
  tell $ BS.pack bytes
  modify (incrementLoc . fromIntegral $ length bytes)

class Bytes a where
  defb :: a -> Z80ASM

instance Bytes ByteString where
  defb = defByteString
instance (b ~ Word8) => Bytes [b] where
  defb bs = Z80 $ do
      tell $ BS.pack bs
      modify (incrementLoc . fromIntegral $ length bs)
        -- The new location has to be computed lazily in the actual
        -- content of the bytes, so that we can emit byte values
        -- referring to later labels.

db :: Bytes a => a -> Z80ASM
db = defb

defByteString :: ByteString -> Z80ASM
defByteString bs = Z80 $ do
  tell bs
  modify (incrementLoc . fromIntegral $ BS.length bs)

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
  where setEntry l st@(ASMState _ Nothing)  = st { entry = Just l }
        setEntry l st@(ASMState _ (Just e)) =
          error $ "Cannot set execution start point twice.  First start point: " ++ show e ++
                  " This start point: " ++ show l

org :: Location -> Z80ASM -> ASMBlock
org addr (Z80 mc) = ASMBlock { asmOrg = addr,
                               asmEntry = fromMaybe addr $ entry finalState,
                               asmData = asm }
 where ((), finalState, asm) = runRWS mc () (ASMState addr Nothing)

equ :: a -> Z80 a
equ = return
