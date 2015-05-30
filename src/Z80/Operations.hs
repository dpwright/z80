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
    -- * Exchange, Block Transfer, and Search Group
  , ex
  , exx
  , ldi
  , ldir
  , ldd
  , lddr
  , cpi
  , cpir
  , cpd
  , cpdr
    -- * 8-Bit Arithmetic Group
  , sub
  , and
  , or
  , xor
  , cp
  , inc
  , dec
    -- * General-Purpose Arithmetic and CPU Control Groups
  , daa
  , cpl
  , neg
  , ccf
  , scf
  , nop
  , halt
  , di
  , ei
  , im
    -- * 8-Bit / 16-Bit Arithmetic Group
  , add
  , adc
  , sbc
  ) where

import Data.Bits hiding (xor)
import Data.Word
import Data.ByteString

import Z80.Assembler
import Z80.Operands

import Prelude hiding (and, or)

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



class Exchange reg reg' where
  ex :: reg -> reg' -> Z80ASM

instance Exchange DE HL where
  ex DE HL = db $ pack [0xeb]

instance Exchange AF AF' where
  ex AF AF' = db $ pack [0x08]

instance Exchange [SP] HL where
  ex [SP] HL = db $ pack [0xe3]
  ex x    _  = derefError x

instance Exchange [SP] RegIx where
  ex [SP] IX = db $ pack [0xdd, 0xe3]
  ex [SP] IY = db $ pack [0xfd, 0xe3]
  ex x    _  = derefError x

exx :: Z80ASM
exx = db $ pack [0xd9]

ldi :: Z80ASM
ldi = db $ pack [0xed, 0xa0]

ldir :: Z80ASM
ldir = db $ pack [0xed, 0xb0]

ldd :: Z80ASM
ldd = db $ pack [0xed, 0xa8]

lddr :: Z80ASM
lddr = db $ pack [0xed, 0xb8]

cpi :: Z80ASM
cpi = db $ pack [0xed, 0xa1]

cpir :: Z80ASM
cpir = db $ pack [0xed, 0xb1]

cpd :: Z80ASM
cpd = db $ pack [0xed, 0xa9]

cpdr :: Z80ASM
cpdr = db $ pack [0xed, 0xb9]



class Arithmetic8 operand where
  sub :: operand -> Z80ASM
  and :: operand -> Z80ASM
  or  :: operand -> Z80ASM
  xor :: operand -> Z80ASM
  cp  :: operand -> Z80ASM

instance Arithmetic8 A where
  sub   r = db $ pack [0x1 .<. 7 .|. 0x2 .<. 3 .|. encode r]
  and   r = db $ pack [0x1 .<. 7 .|. 0x4 .<. 3 .|. encode r]
  or    r = db $ pack [0x1 .<. 7 .|. 0x6 .<. 3 .|. encode r]
  xor   r = db $ pack [0x1 .<. 7 .|. 0x5 .<. 3 .|. encode r]
  cp    r = db $ pack [0x1 .<. 7 .|. 0x7 .<. 3 .|. encode r]

instance Arithmetic8 Reg8 where
  sub   r = db $ pack [0x1 .<. 7 .|. 0x2 .<. 3 .|. encode r]
  and   r = db $ pack [0x1 .<. 7 .|. 0x4 .<. 3 .|. encode r]
  or    r = db $ pack [0x1 .<. 7 .|. 0x6 .<. 3 .|. encode r]
  xor   r = db $ pack [0x1 .<. 7 .|. 0x5 .<. 3 .|. encode r]
  cp    r = db $ pack [0x1 .<. 7 .|. 0x7 .<. 3 .|. encode r]

instance (n ~ Word8) => Arithmetic8 n where
  sub   n = db $ pack [0xd6, n]
  and   n = db $ pack [0xe6, n]
  or    n = db $ pack [0xf6, n]
  xor   n = db $ pack [0xee, n]
  cp    n = db $ pack [0xfe, n]

instance Arithmetic8 [HL] where
  sub   [HL] = db $ pack [0x96]
  sub   x    = derefError x
  and   [HL] = db $ pack [0xa6]
  and   x    = derefError x
  or    [HL] = db $ pack [0xb6]
  or    x    = derefError x
  xor   [HL] = db $ pack [0xae]
  xor   x    = derefError x
  cp    [HL] = db $ pack [0xbe]
  cp    x    = derefError x

instance Arithmetic8 [IxOffset] where
  sub   [IX :+ d] = db $ pack [0xdd, 0x96, d]
  sub   [IY :+ d] = db $ pack [0xfd, 0x96, d]
  sub   x         = derefError x
  and   [IX :+ d] = db $ pack [0xdd, 0xa6, d]
  and   [IY :+ d] = db $ pack [0xfd, 0xa6, d]
  and   x         = derefError x
  or    [IX :+ d] = db $ pack [0xdd, 0xb6, d]
  or    [IY :+ d] = db $ pack [0xfd, 0xb6, d]
  or    x         = derefError x
  xor   [IX :+ d] = db $ pack [0xdd, 0xae, d]
  xor   [IY :+ d] = db $ pack [0xfd, 0xae, d]
  xor   x         = derefError x
  cp    [IX :+ d] = db $ pack [0xdd, 0xbe, d]
  cp    [IY :+ d] = db $ pack [0xfd, 0xbe, d]
  cp    x         = derefError x

class Inc operand where
  inc :: operand -> Z80ASM
  dec :: operand -> Z80ASM

instance Inc A where
  inc r = db $ pack [encode r .<. 3 .|. 0x4]
  dec r = db $ pack [encode r .<. 3 .|. 0x5]

instance Inc Reg8 where
  inc r = db $ pack [encode r .<. 3 .|. 0x4]
  dec r = db $ pack [encode r .<. 3 .|. 0x5]

instance Inc [HL] where
  inc [HL] = db $ pack [0x34]
  inc x    = derefError x
  dec [HL] = db $ pack [0x35]
  dec x    = derefError x

instance Inc [IxOffset] where
  inc [IX :+ d] = db $ pack [0xdd, 0x34, d]
  inc [IY :+ d] = db $ pack [0xfd, 0x34, d]
  inc x         = derefError x
  dec [IX :+ d] = db $ pack [0xdd, 0x35, d]
  dec [IY :+ d] = db $ pack [0xfd, 0x35, d]
  dec x         = derefError x



daa :: Z80ASM
daa = db $ pack [0x27]

cpl :: Z80ASM
cpl = db $ pack [0x2f]

neg :: Z80ASM
neg = db $ pack [0xed, 0x44]

ccf :: Z80ASM
ccf = db $ pack [0x3f]

scf :: Z80ASM
scf = db $ pack [0x37]

nop :: Z80ASM
nop = db $ pack [0x00]

halt :: Z80ASM
halt = db $ pack [0x76]

di :: Z80ASM
di = db $ pack [0xf3]

ei :: Z80ASM
ei = db $ pack [0xfb]

im :: Word8 -> Z80ASM
im 0 = db $ pack [0xed, 0x46]
im 1 = db $ pack [0xed, 0x56]
im 2 = db $ pack [0xed, 0x5e]
im x = error $ "Invalid interrupt mode: " ++ show x



class Arithmetic target operand where
  add :: target -> operand -> Z80ASM

class CarryArithmetic target operand where
  adc :: target -> operand -> Z80ASM
  sbc :: target -> operand -> Z80ASM

instance Arithmetic A A where
  add A r = db $ pack [0x1 .<. 7 .|. encode r]

instance CarryArithmetic A A where
  adc A r = db $ pack [0x1 .<. 7 .|. 0x1 .<. 3 .|. encode r]
  sbc A r = db $ pack [0x1 .<. 7 .|. 0x3 .<. 3 .|. encode r]

instance Arithmetic A Reg8 where
  add A r = db $ pack [0x1 .<. 7 .|. encode r]

instance CarryArithmetic A Reg8 where
  adc A r = db $ pack [0x1 .<. 7 .|. 0x1 .<. 3 .|. encode r]
  sbc A r = db $ pack [0x1 .<. 7 .|. 0x3 .<. 3 .|. encode r]

instance (n ~ Word8) => Arithmetic A n where
  add A n = db $ pack [0xc6, n]

instance (n ~ Word8) => CarryArithmetic A n where
  adc A n = db $ pack [0xce, n]
  sbc A n = db $ pack [0xde, n]

instance Arithmetic A [HL] where
  add A [HL] = db $ pack [0x86]
  add A x    = derefError x

instance CarryArithmetic A [HL] where
  adc A [HL] = db $ pack [0x8e]
  adc A x    = derefError x
  sbc A [HL] = db $ pack [0x9e]
  sbc A x    = derefError x

instance Arithmetic A [IxOffset] where
  add A [IX :+ d] = db $ pack [0xdd, 0x86, d]
  add A [IY :+ d] = db $ pack [0xfd, 0x86, d]
  add A x         = derefError x

instance CarryArithmetic A [IxOffset] where
  adc A [IX :+ d] = db $ pack [0xdd, 0x8e, d]
  adc A [IY :+ d] = db $ pack [0xfd, 0x8e, d]
  adc A x         = derefError x
  sbc A [IX :+ d] = db $ pack [0xdd, 0x9e, d]
  sbc A [IY :+ d] = db $ pack [0xfd, 0x9e, d]
  sbc A x         = derefError x

instance (Reg16 ss) => Arithmetic HL ss where
  add HL ss = db $ pack [encode ss .<. 4 .|. 0x09]

instance (Reg16 ss) => CarryArithmetic HL ss where
  adc HL ss = db $ pack [0xed, 0x40 .|. encode ss .<. 4 .|. 0x0a]
  sbc HL ss = db $ pack [0xed, 0x40 .|. encode ss .<. 4 .|. 0x2]

instance (Reg16 ss, Show ss) => Arithmetic RegIx ss where
  add IX pp = db $ pack [0xdd, encode pp .<. 4 .|. 0x09]
  add IY pp = db $ pack [0xfd, encode pp .<. 4 .|. 0x09]
instance Arithmetic RegIx SP where
  add IX pp = db $ pack [0xdd, encode pp .<. 4 .|. 0x09]
  add IY pp = db $ pack [0xfd, encode pp .<. 4 .|. 0x09]
instance Arithmetic RegIx RegIx where
  add IX IX = db $ pack [0xdd, 0x2 .<. 4 .|. 0x09]
  add IY IY = db $ pack [0xfd, 0x2 .<. 4 .|. 0x09]
  add i  i' = error $ "Invalid operation: add " ++ show i ++ " " ++ show i'

instance (Reg16 ss) => Inc ss where
  inc r = db $ pack [encode r .<. 4 .|. 0x3]
  dec r = db $ pack [encode r .<. 4 .|. 0xb]
instance Inc HL where
  inc r = db $ pack [encode r .<. 4 .|. 0x3]
  dec r = db $ pack [encode r .<. 4 .|. 0xb]
instance Inc SP where
  inc r = db $ pack [encode r .<. 4 .|. 0x3]
  dec r = db $ pack [encode r .<. 4 .|. 0xb]
instance Inc RegIx where
  inc IX = db $ pack [0xdd, 0x23]
  inc IY = db $ pack [0xfd, 0x23]
  dec IX = db $ pack [0xdd, 0x2b]
  dec IY = db $ pack [0xfd, 0x2b]

{- -------- INTERNAL UTILITIES -------- -}

(.<.) :: Bits a => a -> Int -> a
(.<.) = shiftL

hi, lo :: Word16 -> Word8
hi = fromIntegral . byteSwap16
lo = fromIntegral

derefError :: Show a => a -> b
derefError x = error $ "Dereference syntax is a list with exactly one entry. Invalid: " ++ show x

