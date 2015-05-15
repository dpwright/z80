{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Z80.Operations
  ( -- * Load Group
    ld
  , push
  , pop
  ) where

import Data.Bits
import Data.Word
import Data.ByteString

import Z80.Assembler
import Z80.Operands

class Load tgt src where
  ld :: tgt -> src -> Z80ASM

instance Load Reg8 Reg8 where
  ld r r' = db $ pack [1 .<. 6 .|. encode r .<. 3 .|. encode r']
instance Load A Reg8 where
  ld r r' = db $ pack [1 .<. 6 .|. encode r .<. 3 .|. encode r']
instance Load Reg8 A where
  ld r r' = db $ pack [1 .<. 6 .|. encode r .<. 3 .|. encode r']

instance (n ~ Word16) => Load Reg8 n where
  ld r n = db $ pack [encode r .<. 3 .|. 6, fromIntegral n]
instance (n ~ Word16) => Load A n where
  ld r n = db $ pack [encode r .<. 3 .|. 6, fromIntegral n]

instance Load Reg8 [HL] where
  ld r [HL] = db $ pack [1 .<. 6 .|. encode r .<. 3 .|. 6]
  ld _ x = derefError x
instance Load A [HL] where
  ld r [HL] = db $ pack [1 .<. 6 .|. encode r .<. 3 .|. 6]
  ld _ x = derefError x
instance Load [HL] Reg8 where
  ld [HL] r = db $ pack [1 .<. 6 .|. 6 .<. 3 .|. encode r]
  ld x _ = derefError x
instance Load [HL] A where
  ld [HL] r = db $ pack [1 .<. 6 .|. 6 .<. 3 .|. encode r]
  ld x _ = derefError x
instance (n ~ Word8) => Load [HL] n where
  ld [HL] n = db $ pack [0x36, n]
  ld x _ = derefError x

instance Load Reg8 [IxOffset] where
  ld r [IX :+ ofst] = db $
    pack [0xdd, 1 .<. 6 .|. encode r .<. 3 .|. 6, ofst]
  ld r [IY :+ ofst] = db $
    pack [0xfd, 1 .<. 6 .|. encode r .<. 3 .|. 6, ofst]
  ld _ x = derefError x
instance Load A [IxOffset] where
  ld r [IX :+ ofst] = db $
    pack [0xdd, 1 .<. 6 .|. encode r .<. 3 .|. 6, ofst]
  ld r [IY :+ ofst] = db $
    pack [0xfd, 1 .<. 6 .|. encode r .<. 3 .|. 6, ofst]
  ld _ x = derefError x
instance Load [IxOffset] Reg8 where
  ld [IX :+ ofst] r = db $
    pack [0xdd, 1 .<. 6 .|. 6 .<. 3 .|. encode r, ofst]
  ld [IY :+ ofst] r = db $
    pack [0xfd, 1 .<. 6 .|. 6 .<. 3 .|. encode r, ofst]
  ld x _ = derefError x
instance Load [IxOffset] A where
  ld [IX :+ ofst] r = db $
    pack [0xdd, 1 .<. 6 .|. 6 .<. 3 .|. encode r, ofst]
  ld [IY :+ ofst] r = db $
    pack [0xfd, 1 .<. 6 .|. 6 .<. 3 .|. encode r, ofst]
  ld x _ = derefError x
instance (n ~ Word8) => Load [IxOffset] n where
  ld [IX :+ ofst] n = db $
    pack [0xdd, 0x36, ofst, n]
  ld [IY :+ ofst] n = db $
    pack [0xfd, 0x36, ofst, n]
  ld x _ = derefError x

instance Load A [BC] where
  ld A [BC] = db $ pack [0x0a]
  ld _ x = derefError x
instance Load A [DE] where
  ld A [DE] = db $ pack [0x1a]
  ld _ x = derefError x
instance Load [BC] A where
  ld [BC] A = db $ pack [0x02]
  ld x _ = derefError x
instance Load [DE] A where
  ld [DE] A = db $ pack [0x12]
  ld x _ = derefError x

instance (nn ~ Word16) => Load A [nn] where
  ld A [nn] = db $ pack [0x3a, lo nn, hi nn]
  ld _ x = derefError x
instance (nn ~ Word16) => Load [nn] A where
  ld [nn] A = db $ pack [0x32, lo nn, hi nn]
  ld x _ = derefError x

instance Load A I where
  ld A I = db $ pack [0xed, 0x57]
instance Load I A where
  ld I A = db $ pack [0xed, 0x47]
instance Load A R where
  ld A R = db $ pack [0xed, 0x5f]
instance Load R A where
  ld R A = db $ pack [0xed, 0x4f]

instance (Reg16 dd, nn ~ Word16) => Load dd nn where
  ld dd nn = db $ pack [encode dd .<. 4 .|. 0x01, lo nn, hi nn]
instance (nn ~ Word16) => Load HL nn where
  ld dd nn = db $ pack [encode dd .<. 4 .|. 0x01, lo nn, hi nn]
instance (nn ~ Word16) => Load SP nn where
  ld dd nn = db $ pack [encode dd .<. 4 .|. 0x01, lo nn, hi nn]

instance (nn ~ Word16) => Load RegIx nn where
  ld IX nn = db $ pack [0xdd, 0x21, lo nn, hi nn]
  ld IY nn = db $ pack [0xfd, 0x21, lo nn, hi nn]

instance (nn ~ Word16) => Load HL [nn] where
  ld HL [nn] = db $ pack [0x2a, lo nn, hi nn]
  ld _ x = derefError x
instance (nn ~ Word16) => Load [nn] HL where
  ld [nn] HL = db $ pack [0x22, lo nn, hi nn]
  ld x _ = derefError x

-- NOTE Z80 documentation says you should be able to pass HL here, but that
--      seems to clash with the previous instance, which is HL-specific?
instance (Reg16 dd, nn ~ Word16) => Load dd [nn] where
  ld dd [nn] = db $ pack [0xed, 0x01 .<. 6 .|. encode dd .<. 4 .|. 0xb, lo nn, hi nn]
  ld _ x = derefError x
instance (nn ~ Word16) => Load SP [nn] where
  ld dd [nn] = db $ pack [0xed, 0x01 .<. 6 .|. encode dd .<. 4 .|. 0xb, lo nn, hi nn]
  ld _ x = derefError x
instance (Reg16 dd, nn ~ Word16) => Load [nn] dd where
  ld [nn] dd = db $ pack [0xed, 0x01 .<. 6 .|. encode dd .<. 4 .|. 0x3, lo nn, hi nn]
  ld x _ = derefError x
instance (nn ~ Word16) => Load [nn] SP where
  ld [nn] dd = db $ pack [0xed, 0x01 .<. 6 .|. encode dd .<. 4 .|. 0x3, lo nn, hi nn]
  ld x _ = derefError x

instance (nn ~ Word16) => Load RegIx [nn] where
  ld IX [nn] = db $ pack [0xdd, 0x2a, lo nn, hi nn]
  ld IY [nn] = db $ pack [0xfd, 0x2a, lo nn, hi nn]
  ld _ x = derefError x
instance (nn ~ Word16) => Load [nn] RegIx where
  ld [nn] IX = db $ pack [0xdd, 0x22, lo $ fromIntegral nn, hi $ fromIntegral nn]
  ld [nn] IY = db $ pack [0xfd, 0x22, lo $ fromIntegral nn, hi $ fromIntegral nn]
  ld x _ = derefError x

instance Load SP HL where
  ld SP HL = db $ pack [0xf9]

instance Load SP RegIx where
  ld SP IX = db $ pack [0xdd, 0xf9]
  ld SP IY = db $ pack [0xfd, 0xf9]



class Stack reg where
  push :: reg -> Z80ASM
  pop  :: reg -> Z80ASM

instance (Reg16 qq) => Stack qq where
  push qq = db $ pack [0x3 .<. 6 .|. encode qq .<. 4 .|. 0x5]
  pop  qq = db $ pack [0x3 .<. 6 .|. encode qq .<. 4 .|. 0x1]
instance Stack HL where
  push qq = db $ pack [0x3 .<. 6 .|. encode qq .<. 4 .|. 0x5]
  pop  qq = db $ pack [0x3 .<. 6 .|. encode qq .<. 4 .|. 0x1]
instance Stack AF where
  push qq = db $ pack [0x3 .<. 6 .|. encode qq .<. 4 .|. 0x5]
  pop  qq = db $ pack [0x3 .<. 6 .|. encode qq .<. 4 .|. 0x1]
instance Stack RegIx where
  push IX = db $ pack [0xdd, 0xe5]
  push IY = db $ pack [0xfd, 0xe5]
  pop  IX = db $ pack [0xdd, 0xe1]
  pop  IY = db $ pack [0xfd, 0xe1]

(.<.) :: Bits a => a -> Int -> a
(.<.) = shiftL

hi, lo :: Word16 -> Word8
hi = fromIntegral . byteSwap16
lo = fromIntegral

derefError :: Show a => a -> b
derefError x = error $ "Dereference syntax is a list with exactly one entry. Invalid: " ++ show x

