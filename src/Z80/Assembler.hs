{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
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
  , static
  , end
  ) where

import Data.Word
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Maybe

import Control.Monad.RWS

import Control.Applicative
import Data.Traversable (traverse)
import Prelude

import Z80.Operands

data ASMState
  = ASMState
  { loc        :: Location
  , statics    :: Map String Z80ASM
  , staticLocs :: Map String Location
  }

newtype Z80 a = Z80 { runZ80 :: RWS () ByteString ASMState a }
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
  defb = defByteString . BS.pack

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

static :: String -> Z80ASM -> Z80 Location
static name asm = do
  Z80 $ modify addIfMissing
  st <- Z80 get
  let loc           = M.lookup name $ staticLocs st
      notFoundError = error $ "Static \"" ++ name ++ "\" not found.  This is probably a bug in the z80 package, please report.\n" ++
                              "Statics: " ++ show (staticLocs st)
  return $ fromMaybe notFoundError loc
  where addIfMissing st = if M.member name $ statics st then st
                          else st { statics = M.insert name asm $ statics st }

end :: Z80ASM
end = return ()

org :: Location -> Z80ASM -> ASMBlock
org addr userASM = ASMBlock { asmOrg = addr, asmEntry = addr, asmData = asm }
 where ((), _, asm)      = runRWS (runZ80 runWithStatics) () initialState
       initialState      = ASMState addr M.empty M.empty
       putStatics sls st = st { staticLocs = sls }
       runWithStatics    = mdo
         Z80 . modify $ putStatics labelledStatics
         userASM
         collectedStatics <- statics <$> Z80 get
         labelledStatics  <- traverse labelled collectedStatics
         end

equ :: a -> Z80 a
equ = return
