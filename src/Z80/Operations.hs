{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

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
    -- * Rotate and Shift Group
  , rlca
  , rla
  , rrca
  , rra
  , rlc
  , rl
  , rrc
  , rr
  , sla
  , sra
  , srl
  , rld
  , rrd
    -- * Bit Set, Reset, and Test Group
  , bit
  , set
  , res
    -- * Jump Group
  , jp
  , jr
  , djnz
  , ($-)
  , ($+)
    -- * Call and Return Group
  , call
  , ret
  , reti
  , retn
  , rst
    -- * Input and Output Group
  , in_
  , ini
  , inir
  , ind
  , indr
  , out
  , outi
  , otir
  , outd
  , otdr
  ) where

import Data.Bits hiding (xor, bit)
import Data.Word

import Z80.Assembler
import Z80.Operands

import Control.Applicative ((<$>))
import Control.Monad       ((>=>))
import Prelude hiding      (and, or)

class Load tgt src where
  ld :: tgt -> src -> Z80ASM

instance Load Reg8 Reg8 where
  ld r r' = code [1 .<. 6 .|. encodeReg8 r .<. 3 .|. encodeReg8 r']
instance Load Reg8 C where
  ld r r' = code [1 .<. 6 .|. encodeReg8 r .<. 3 .|. encodeReg8 r']
instance Load C Reg8 where
  ld r r' = code [1 .<. 6 .|. encodeReg8 r .<. 3 .|. encodeReg8 r']
instance Load A Reg8 where
  ld r r' = code [1 .<. 6 .|. encodeReg8 r .<. 3 .|. encodeReg8 r']
instance Load Reg8 A where
  ld r r' = code [1 .<. 6 .|. encodeReg8 r .<. 3 .|. encodeReg8 r']
instance Load A C where
  ld r r' = code [1 .<. 6 .|. encodeReg8 r .<. 3 .|. encodeReg8 r']
instance Load C A where
  ld r r' = code [1 .<. 6 .|. encodeReg8 r .<. 3 .|. encodeReg8 r']

instance (n ~ Word16) => Load Reg8 n where
  ld r n = code [encodeReg8 r .<. 3 .|. 6, fromIntegral n]
instance (n ~ Word16) => Load A n where
  ld r n = code [encodeReg8 r .<. 3 .|. 6, fromIntegral n]
instance (n ~ Word16) => Load C n where
  ld r n = code [encodeReg8 r .<. 3 .|. 6, fromIntegral n]

instance Load Reg8 [HL] where
  ld r [HL] = code [1 .<. 6 .|. encodeReg8 r .<. 3 .|. 6]
  ld _ x = derefError x
instance Load A [HL] where
  ld r [HL] = code [1 .<. 6 .|. encodeReg8 r .<. 3 .|. 6]
  ld _ x = derefError x
instance Load C [HL] where
  ld r [HL] = code [1 .<. 6 .|. encodeReg8 r .<. 3 .|. 6]
  ld _ x = derefError x
instance Load [HL] Reg8 where
  ld [HL] r = code [1 .<. 6 .|. 6 .<. 3 .|. encodeReg8 r]
  ld x _ = derefError x
instance Load [HL] A where
  ld [HL] r = code [1 .<. 6 .|. 6 .<. 3 .|. encodeReg8 r]
  ld x _ = derefError x
instance Load [HL] C where
  ld [HL] r = code [1 .<. 6 .|. 6 .<. 3 .|. encodeReg8 r]
  ld x _ = derefError x
instance (n ~ Word8) => Load [HL] n where
  ld [HL] n = code [0x36, n]
  ld x _ = derefError x

instance Load Reg8 [RegIx] where
  ld r [i :+ ofst] =
    code [encodeOrError i, 1 .<. 6 .|. encodeReg8 r .<. 3 .|. 6, ofst]
  ld r [i] = ld r [i+0]
  ld _ x   = derefError x
instance Load A [RegIx] where
  ld r [i :+ ofst] =
    code [encodeOrError i, 1 .<. 6 .|. encodeReg8 r .<. 3 .|. 6, ofst]
  ld r [i] = ld r [i+0]
  ld _ x   = derefError x
instance Load C [RegIx] where
  ld r [i :+ ofst] =
    code [encodeOrError i, 1 .<. 6 .|. encodeReg8 r .<. 3 .|. 6, ofst]
  ld r [i] = ld r [i+0]
  ld _ x   = derefError x
instance Load [RegIx] Reg8 where
  ld [i :+ ofst] r =
    code [encodeOrError i, 1 .<. 6 .|. 6 .<. 3 .|. encodeReg8 r, ofst]
  ld [i] r = ld [i+0] r
  ld x   _ = derefError x
instance Load [RegIx] A where
  ld [i :+ ofst] r =
    code [encodeOrError i, 1 .<. 6 .|. 6 .<. 3 .|. encodeReg8 r, ofst]
  ld [i] r = ld [i+0] r
  ld x   _ = derefError x
instance Load [RegIx] C where
  ld [i :+ ofst] r =
    code [encodeOrError i, 1 .<. 6 .|. 6 .<. 3 .|. encodeReg8 r, ofst]
  ld [i] r = ld [i+0] r
  ld x   _ = derefError x
instance (n ~ Word8) => Load [RegIx] n where
  ld [i :+ ofst] n =
    code [encodeOrError i, 0x36, ofst, n]
  ld [i] n = ld [i+0] n
  ld x   _ = derefError x

instance Load A [BC] where
  ld A [BC] = code [0x0a]
  ld _ x = derefError x
instance Load A [DE] where
  ld A [DE] = code [0x1a]
  ld _ x = derefError x
instance Load [BC] A where
  ld [BC] A = code [0x02]
  ld x _ = derefError x
instance Load [DE] A where
  ld [DE] A = code [0x12]
  ld x _ = derefError x

instance (nn ~ Word16) => Load A [nn] where
  ld A [nn] = code [0x3a, lo nn, hi nn]
  ld _ x = derefError x
instance (nn ~ Word16) => Load [nn] A where
  ld [nn] A = code [0x32, lo nn, hi nn]
  ld x _ = derefError x

instance Load A I where
  ld A I = code [0xed, 0x57]
instance Load I A where
  ld I A = code [0xed, 0x47]
instance Load A R where
  ld A R = code [0xed, 0x5f]
instance Load R A where
  ld R A = code [0xed, 0x4f]

instance (Reg16 dd, nn ~ Word16) => Load dd nn where
  ld dd nn = code [encodeReg16 dd .<. 4 .|. 0x01, lo nn, hi nn]
instance (nn ~ Word16) => Load HL nn where
  ld dd nn = code [encodeReg16 dd .<. 4 .|. 0x01, lo nn, hi nn]
instance (nn ~ Word16) => Load SP nn where
  ld dd nn = code [encodeReg16 dd .<. 4 .|. 0x01, lo nn, hi nn]

instance (nn ~ Word16) => Load RegIx nn where
  ld i nn = code [encodeReg16 i, 0x21, lo nn, hi nn]

instance (nn ~ Word16) => Load HL [nn] where
  ld HL [nn] = code [0x2a, lo nn, hi nn]
  ld _ x = derefError x
instance (nn ~ Word16) => Load [nn] HL where
  ld [nn] HL = code [0x22, lo nn, hi nn]
  ld x _ = derefError x

-- NOTE Z80 documentation says you should be able to pass HL here, but that
--      seems to clash with the previous instance, which is HL-specific?
instance (Reg16 dd, nn ~ Word16) => Load dd [nn] where
  ld dd [nn] = code [0xed, 0x01 .<. 6 .|. encodeReg16 dd .<. 4 .|. 0xb, lo nn, hi nn]
  ld _ x = derefError x
instance (nn ~ Word16) => Load SP [nn] where

  ld dd [nn] = code [0xed, 0x01 .<. 6 .|. encodeReg16 dd .<. 4 .|. 0xb, lo nn, hi nn]
  ld _ x = derefError x
instance (Reg16 dd, nn ~ Word16) => Load [nn] dd where
  ld [nn] dd = code [0xed, 0x01 .<. 6 .|. encodeReg16 dd .<. 4 .|. 0x3, lo nn, hi nn]
  ld x _ = derefError x
instance (nn ~ Word16) => Load [nn] SP where
  ld [nn] dd = code [0xed, 0x01 .<. 6 .|. encodeReg16 dd .<. 4 .|. 0x3, lo nn, hi nn]
  ld x _ = derefError x

instance (nn ~ Word16) => Load RegIx [nn] where
  ld i [nn] = code [encodeReg16 i, 0x2a, lo nn, hi nn]
  ld _ x    = derefError x
instance (nn ~ Word16) => Load [nn] RegIx where
  ld [nn] i = code [encodeReg16 i, 0x22, lo $ fromIntegral nn, hi $ fromIntegral nn]
  ld x _    = derefError x

instance Load SP HL where
  ld SP HL = code [0xf9]

instance Load SP RegIx where
  ld SP i = code [encodeReg16 i, 0xf9]



class Stack reg where
  push :: reg -> Z80ASM
  pop  :: reg -> Z80ASM

instance (Reg16 qq) => Stack qq where
  push qq = code [0x3 .<. 6 .|. encodeReg16 qq .<. 4 .|. 0x5]
  pop  qq = code [0x3 .<. 6 .|. encodeReg16 qq .<. 4 .|. 0x1]
instance Stack HL where
  push qq = code [0x3 .<. 6 .|. encodeReg16 qq .<. 4 .|. 0x5]
  pop  qq = code [0x3 .<. 6 .|. encodeReg16 qq .<. 4 .|. 0x1]
instance Stack AF where
  push qq = code [0x3 .<. 6 .|. encodeReg16 qq .<. 4 .|. 0x5]
  pop  qq = code [0x3 .<. 6 .|. encodeReg16 qq .<. 4 .|. 0x1]
instance Stack RegIx where
  push i = code [encodeReg16 i, 0xe5]
  pop  i = code [encodeReg16 i, 0xe1]



class Exchange reg reg' where
  ex :: reg -> reg' -> Z80ASM

instance Exchange DE HL where
  ex DE HL = code [0xeb]

instance Exchange AF AF' where
  ex AF AF' = code [0x08]

instance Exchange [SP] HL where
  ex [SP] HL = code [0xe3]
  ex x    _  = derefError x

instance Exchange [SP] RegIx where
  ex [SP] i = code [encodeReg16 i, 0xe3]
  ex x    _ = derefError x

exx :: Z80ASM
exx = code [0xd9]

ldi :: Z80ASM
ldi = code [0xed, 0xa0]

ldir :: Z80ASM
ldir = code [0xed, 0xb0]

ldd :: Z80ASM
ldd = code [0xed, 0xa8]

lddr :: Z80ASM
lddr = code [0xed, 0xb8]

cpi :: Z80ASM
cpi = code [0xed, 0xa1]

cpir :: Z80ASM
cpir = code [0xed, 0xb1]

cpd :: Z80ASM
cpd = code [0xed, 0xa9]

cpdr :: Z80ASM
cpdr = code [0xed, 0xb9]



class Arithmetic8 operand where
  sub :: operand -> Z80ASM
  and :: operand -> Z80ASM
  or  :: operand -> Z80ASM
  xor :: operand -> Z80ASM
  cp  :: operand -> Z80ASM

instance Arithmetic8 A where
  sub   r = code [0x1 .<. 7 .|. 0x2 .<. 3 .|. encodeReg8 r]
  and   r = code [0x1 .<. 7 .|. 0x4 .<. 3 .|. encodeReg8 r]
  or    r = code [0x1 .<. 7 .|. 0x6 .<. 3 .|. encodeReg8 r]
  xor   r = code [0x1 .<. 7 .|. 0x5 .<. 3 .|. encodeReg8 r]
  cp    r = code [0x1 .<. 7 .|. 0x7 .<. 3 .|. encodeReg8 r]

instance Arithmetic8 C where
  sub   r = code [0x1 .<. 7 .|. 0x2 .<. 3 .|. encodeReg8 r]
  and   r = code [0x1 .<. 7 .|. 0x4 .<. 3 .|. encodeReg8 r]
  or    r = code [0x1 .<. 7 .|. 0x6 .<. 3 .|. encodeReg8 r]
  xor   r = code [0x1 .<. 7 .|. 0x5 .<. 3 .|. encodeReg8 r]
  cp    r = code [0x1 .<. 7 .|. 0x7 .<. 3 .|. encodeReg8 r]

instance Arithmetic8 Reg8 where
  sub   r = code [0x1 .<. 7 .|. 0x2 .<. 3 .|. encodeReg8 r]
  and   r = code [0x1 .<. 7 .|. 0x4 .<. 3 .|. encodeReg8 r]
  or    r = code [0x1 .<. 7 .|. 0x6 .<. 3 .|. encodeReg8 r]
  xor   r = code [0x1 .<. 7 .|. 0x5 .<. 3 .|. encodeReg8 r]
  cp    r = code [0x1 .<. 7 .|. 0x7 .<. 3 .|. encodeReg8 r]

instance (n ~ Word8) => Arithmetic8 n where
  sub   n = code [0xd6, n]
  and   n = code [0xe6, n]
  or    n = code [0xf6, n]
  xor   n = code [0xee, n]
  cp    n = code [0xfe, n]

instance Arithmetic8 [HL] where
  sub   [HL] = code [0x96]
  sub   x    = derefError x
  and   [HL] = code [0xa6]
  and   x    = derefError x
  or    [HL] = code [0xb6]
  or    x    = derefError x
  xor   [HL] = code [0xae]
  xor   x    = derefError x
  cp    [HL] = code [0xbe]
  cp    x    = derefError x

instance Arithmetic8 [RegIx] where
  sub   [i :+ d] = code [encodeOrError i, 0x96, d]
  sub   [i]      = sub [i+0]
  sub   x        = derefError x
  and   [i :+ d] = code [encodeOrError i, 0xa6, d]
  and   [i]      = and [i+0]
  and   x        = derefError x
  or    [i :+ d] = code [encodeOrError i, 0xb6, d]
  or    [i]      = or [i+0]
  or    x        = derefError x
  xor   [i :+ d] = code [encodeOrError i, 0xae, d]
  xor   [i]      = xor [i+0]
  xor   x        = derefError x
  cp    [i :+ d] = code [encodeOrError i, 0xbe, d]
  cp    [i]      = cp [i+0]
  cp    x        = derefError x

class Inc operand where
  inc :: operand -> Z80ASM
  dec :: operand -> Z80ASM

instance Inc A where
  inc r = code [encodeReg8 r .<. 3 .|. 0x4]
  dec r = code [encodeReg8 r .<. 3 .|. 0x5]

instance Inc C where
  inc r = code [encodeReg8 r .<. 3 .|. 0x4]
  dec r = code [encodeReg8 r .<. 3 .|. 0x5]

instance Inc Reg8 where
  inc r = code [encodeReg8 r .<. 3 .|. 0x4]
  dec r = code [encodeReg8 r .<. 3 .|. 0x5]

instance Inc [HL] where
  inc [HL] = code [0x34]
  inc x    = derefError x
  dec [HL] = code [0x35]
  dec x    = derefError x

instance Inc [RegIx] where
  inc [i :+ d] = code [encodeOrError i, 0x34, d]
  inc [i]      = inc [i+0]
  inc x        = derefError x
  dec [i :+ d] = code [encodeOrError i, 0x35, d]
  dec [i]      = dec [i+0]
  dec x        = derefError x



daa :: Z80ASM
daa = code [0x27]

cpl :: Z80ASM
cpl = code [0x2f]

neg :: Z80ASM
neg = code [0xed, 0x44]

ccf :: Z80ASM
ccf = code [0x3f]

scf :: Z80ASM
scf = code [0x37]

nop :: Z80ASM
nop = code [0x00]

halt :: Z80ASM
halt = code [0x76]

di :: Z80ASM
di = code [0xf3]

ei :: Z80ASM
ei = code [0xfb]

im :: Word8 -> Z80ASM
im 0 = code [0xed, 0x46]
im 1 = code [0xed, 0x56]
im 2 = code [0xed, 0x5e]
im x = error $ "Invalid interrupt mode: " ++ show x



class Arithmetic target operand where
  add :: target -> operand -> Z80ASM

class CarryArithmetic target operand where
  adc :: target -> operand -> Z80ASM
  sbc :: target -> operand -> Z80ASM

instance Arithmetic A A where
  add A r = code [0x1 .<. 7 .|. encodeReg8 r]

instance CarryArithmetic A A where
  adc A r = code [0x1 .<. 7 .|. 0x1 .<. 3 .|. encodeReg8 r]
  sbc A r = code [0x1 .<. 7 .|. 0x3 .<. 3 .|. encodeReg8 r]

instance Arithmetic A C where
  add A r = code [0x1 .<. 7 .|. encodeReg8 r]

instance CarryArithmetic A C where
  adc A r = code [0x1 .<. 7 .|. 0x1 .<. 3 .|. encodeReg8 r]
  sbc A r = code [0x1 .<. 7 .|. 0x3 .<. 3 .|. encodeReg8 r]

instance Arithmetic A Reg8 where
  add A r = code [0x1 .<. 7 .|. encodeReg8 r]

instance CarryArithmetic A Reg8 where
  adc A r = code [0x1 .<. 7 .|. 0x1 .<. 3 .|. encodeReg8 r]
  sbc A r = code [0x1 .<. 7 .|. 0x3 .<. 3 .|. encodeReg8 r]

instance (n ~ Word8) => Arithmetic A n where
  add A n = code [0xc6, n]

instance (n ~ Word8) => CarryArithmetic A n where
  adc A n = code [0xce, n]
  sbc A n = code [0xde, n]

instance Arithmetic A [HL] where
  add A [HL] = code [0x86]
  add A x    = derefError x

instance CarryArithmetic A [HL] where
  adc A [HL] = code [0x8e]
  adc A x    = derefError x
  sbc A [HL] = code [0x9e]
  sbc A x    = derefError x

instance Arithmetic A [RegIx] where
  add A [i :+ d] = code [encodeOrError i, 0x86, d]
  add A [i]      = add A [i+0]
  add A x        = derefError x

instance CarryArithmetic A [RegIx] where
  adc A [i :+ d] = code [encodeOrError i, 0x8e, d]
  adc A [i]      = adc A [i+0]
  adc A x        = derefError x
  sbc A [i :+ d] = code [encodeOrError i, 0x9e, d]
  sbc A [i]      = sbc A [i+0]
  sbc A x        = derefError x

instance (Reg16 ss) => Arithmetic HL ss where
  add HL ss = code [encodeReg16 ss .<. 4 .|. 0x09]

instance (Reg16 ss) => CarryArithmetic HL ss where
  adc HL ss = code [0xed, 0x40 .|. encodeReg16 ss .<. 4 .|. 0x0a]
  sbc HL ss = code [0xed, 0x40 .|. encodeReg16 ss .<. 4 .|. 0x2]

instance (Reg16 ss, Show ss) => Arithmetic RegIx ss where
  add i pp = code [encodeReg16 i, encodeReg16 pp .<. 4 .|. 0x09]
instance Arithmetic RegIx SP where
  add i pp = code [encodeReg16 i, encodeReg16 pp .<. 4 .|. 0x09]
instance Arithmetic RegIx RegIx where
  add i i' | i == i' = code [encodeReg16 i, 0x2 .<. 4 .|. 0x09]
  add i  i'          = error $ "Invalid operation: add " ++ show i ++ " " ++ show i'

instance (Reg16 ss) => Inc ss where
  inc r = code [encodeReg16 r .<. 4 .|. 0x3]
  dec r = code [encodeReg16 r .<. 4 .|. 0xb]
instance Inc HL where
  inc r = code [encodeReg16 r .<. 4 .|. 0x3]
  dec r = code [encodeReg16 r .<. 4 .|. 0xb]
instance Inc SP where
  inc r = code [encodeReg16 r .<. 4 .|. 0x3]
  dec r = code [encodeReg16 r .<. 4 .|. 0xb]
instance Inc RegIx where
  inc i = code [encodeReg16 i, 0x23]
  dec i = code [encodeReg16 i, 0x2b]



rlca, rla, rrca, rra :: Z80ASM
rlca = code [0x07]
rla  = code [0x17]
rrca = code [0x0f]
rra  = code [0x1f]

class RotateShift r where
  rlc :: r -> Z80ASM
  rl  :: r -> Z80ASM
  rrc :: r -> Z80ASM
  rr  :: r -> Z80ASM
  sla :: r -> Z80ASM
  sra :: r -> Z80ASM
  srl :: r -> Z80ASM

instance RotateShift Reg8 where
  rlc r = code [0xcb, encodeReg8 r]
  rl  r = code [0xcb, 0x2 .<. 3 .|. encodeReg8 r]
  rrc r = code [0xcb, 0x1 .<. 3 .|. encodeReg8 r]
  rr  r = code [0xcb, 0x3 .<. 3 .|. encodeReg8 r]
  sla r = code [0xcb, 0x4 .<. 3 .|. encodeReg8 r]
  sra r = code [0xcb, 0x5 .<. 3 .|. encodeReg8 r]
  srl r = code [0xcb, 0x7 .<. 3 .|. encodeReg8 r]
instance RotateShift A where
  rlc r = code [0xcb, encodeReg8 r]
  rl  r = code [0xcb, 0x2 .<. 3 .|. encodeReg8 r]
  rrc r = code [0xcb, 0x1 .<. 3 .|. encodeReg8 r]
  rr  r = code [0xcb, 0x3 .<. 3 .|. encodeReg8 r]
  sla r = code [0xcb, 0x4 .<. 3 .|. encodeReg8 r]
  sra r = code [0xcb, 0x5 .<. 3 .|. encodeReg8 r]
  srl r = code [0xcb, 0x7 .<. 3 .|. encodeReg8 r]
instance RotateShift C where
  rlc r = code [0xcb, encodeReg8 r]
  rl  r = code [0xcb, 0x2 .<. 3 .|. encodeReg8 r]
  rrc r = code [0xcb, 0x1 .<. 3 .|. encodeReg8 r]
  rr  r = code [0xcb, 0x3 .<. 3 .|. encodeReg8 r]
  sla r = code [0xcb, 0x4 .<. 3 .|. encodeReg8 r]
  sra r = code [0xcb, 0x5 .<. 3 .|. encodeReg8 r]
  srl r = code [0xcb, 0x7 .<. 3 .|. encodeReg8 r]

instance RotateShift [HL] where
  rlc [HL] = code [0xcb, 0x06]
  rlc x    = derefError x
  rl  [HL] = code [0xcb, 0x16]
  rl  x    = derefError x
  rrc [HL] = code [0xcb, 0x0e]
  rrc x    = derefError x
  rr  [HL] = code [0xcb, 0x1e]
  rr  x    = derefError x
  sla [HL] = code [0xcb, 0x26]
  sla x    = derefError x
  sra [HL] = code [0xcb, 0x2e]
  sra x    = derefError x
  srl [HL] = code [0xcb, 0x3e]
  srl x    = derefError x

instance RotateShift [RegIx] where
  rlc [i:+d] = code [encodeOrError i, 0xcb, d, 0x06]
  rlc [i]    = rlc [i+0]
  rlc x      = derefError x
  rl  [i:+d] = code [encodeOrError i, 0xcb, d, 0x16]
  rl  [i]    = rl  [i+0]
  rl  x      = derefError x
  rrc [i:+d] = code [encodeOrError i, 0xcb, d, 0x0e]
  rrc [i]    = rrc [i+0]
  rrc x      = derefError x
  rr  [i:+d] = code [encodeOrError i, 0xcb, d, 0x1e]
  rr  [i]    = rr  [i+0]
  rr  x      = derefError x
  sla [i:+d] = code [encodeOrError i, 0xcb, d, 0x26]
  sla [i]    = sla [i+0]
  sla x      = derefError x
  sra [i:+d] = code [encodeOrError i, 0xcb, d, 0x2e]
  sra [i]    = sra [i+0]
  sra x      = derefError x
  srl [i:+d] = code [encodeOrError i, 0xcb, d, 0x3e]
  srl [i]    = srl [i+0]
  srl x      = derefError x

rld, rrd :: Z80ASM
rld = code [0xed, 0x6f]
rrd = code [0xed, 0x67]



class Bitwise r where
  bit :: Word8 -> r -> Z80ASM
  set :: Word8 -> r -> Z80ASM
  res :: Word8 -> r -> Z80ASM

instance Bitwise Reg8 where
  bit b r = code [0xcb, 0x1 .<. 6 .|. b .<. 3 .|. encodeReg8 r]
  set b r = code [0xcb, 0x3 .<. 6 .|. b .<. 3 .|. encodeReg8 r]
  res b r = code [0xcb, 0x2 .<. 6 .|. b .<. 3 .|. encodeReg8 r]

instance Bitwise A where
  bit b r = code [0xcb, 0x1 .<. 6 .|. b .<. 3 .|. encodeReg8 r]
  set b r = code [0xcb, 0x3 .<. 6 .|. b .<. 3 .|. encodeReg8 r]
  res b r = code [0xcb, 0x2 .<. 6 .|. b .<. 3 .|. encodeReg8 r]

instance Bitwise C where
  bit b r = code [0xcb, 0x1 .<. 6 .|. b .<. 3 .|. encodeReg8 r]
  set b r = code [0xcb, 0x3 .<. 6 .|. b .<. 3 .|. encodeReg8 r]
  res b r = code [0xcb, 0x2 .<. 6 .|. b .<. 3 .|. encodeReg8 r]

instance Bitwise [HL] where
  bit b [HL] = code [0xcb, 0x1 .<. 6 .|. b .<. 3 .|. 0x6]
  bit _ x    = derefError x
  set b [HL] = code [0xcb, 0x3 .<. 6 .|. b .<. 3 .|. 0x6]
  set _ x    = derefError x
  res b [HL] = code [0xcb, 0x2 .<. 6 .|. b .<. 3 .|. 0x6]
  res _ x    = derefError x

instance Bitwise [RegIx] where
  bit b [i :+ d] = code [encodeOrError i, 0xcb, d, 0x1 .<. 6 .|. b .<. 3 .|. 0x6]
  bit b [i]      = bit b [i+0]
  bit _ x        = derefError x
  set b [i :+ d] = code [encodeOrError i, 0xcb, d, 0x3 .<. 6 .|. b .<. 3 .|. 0x6]
  set b [i]      = set b [i+0]
  set _ x        = derefError x
  res b [i :+ d] = code [encodeOrError i, 0xcb, d, 0x2 .<. 6 .|. b .<. 3 .|. 0x6]
  res b [i]      = res b [i+0]
  res _ x        = derefError x



class Jump p r where
  jp :: p -> r

instance (nn ~ Word16) => Jump nn (Z80 a) where
  jp nn = meaningless "jp" $ code [0xc3, lo nn, hi nn]

instance (Cond cc, nn ~ Word16) => Jump cc (nn -> Z80 a) where
  jp cc = \nn -> meaningless "jp" $ code [0x3 .<. 6 .|. encodeCondition cc .<. 3 .|. 0x2, lo nn, hi nn]

instance Jump [HL] Z80ASM where
  jp [HL] = code [0xe9]
  jp x    = derefError x

instance Jump [RegIx] Z80ASM where
  jp [i] = code [encodeReg16 i, 0xe9]
  jp x   = derefError x

class JumpRelative p r where
  jr :: p -> r

instance (a ~ Word16) => JumpRelative a Z80ASM where
  jr    = relative >=> \a -> code [0x18, a-2]
instance (a ~ Word16) => JumpRelative C (a -> Z80ASM) where
  jr C  = relative >=> \a -> code [0x38, a-2]
instance (a ~ Word16) => JumpRelative NC (a -> Z80ASM) where
  jr NC = relative >=> \a -> code [0x30, a-2]
instance (a ~ Word16) => JumpRelative Z (a -> Z80ASM) where
  jr Z  = relative >=> \a -> code [0x28, a-2]
instance (a ~ Word16) => JumpRelative NZ (a -> Z80ASM) where
  jr NZ = relative >=> \a -> code [0x20, a-2]

djnz :: Word16 -> Z80ASM
djnz = relative >=> \a -> code [0x10, a-2]

($-), ($+) :: (Word16 -> Z80ASM) -> Word16 -> Z80ASM
op $- a = op . subtract a =<< label
op $+ a = op . (+ a) =<< label



class Call p r where
  call :: p -> r
instance (nn ~ Word16) => Call nn (Z80 a) where
  call nn = meaningless "call" $ code [0xcd, lo nn, hi nn]
instance (nn ~ Word16, Cond cc) => Call cc (nn -> Z80 a) where
  call cc = \nn -> meaningless "call" $ code [0x3 .<. 6 .|. encodeCondition cc .<. 3 .|. 0x4, lo nn, hi nn]

class Return t where
  ret :: t
instance Return (Z80 a) where
  ret = meaningless "call" $ code [0xc9]
instance (Cond cc) => Return (cc -> Z80 a) where
  ret = \cc -> meaningless "call" $ code [0x3 .<. 6 .|. encodeCondition cc .<. 3]

reti, retn :: Z80ASM
reti = code [0xed, 0x4d]
retn = code [0xed, 0x45]

rst :: Word8 -> Z80ASM
rst p = code [0x3 .<. 6 .|. encodeMemLoc p .<. 3 .|. 0x7] where
  encodeMemLoc 0x00 = 0x0 -- 000
  encodeMemLoc 0x08 = 0x1 -- 001
  encodeMemLoc 0x10 = 0x2 -- 010
  encodeMemLoc 0x18 = 0x3 -- 011
  encodeMemLoc 0x20 = 0x4 -- 100
  encodeMemLoc 0x28 = 0x5 -- 101
  encodeMemLoc 0x30 = 0x6 -- 110
  encodeMemLoc 0x38 = 0x7 -- 111
  encodeMemLoc x    = error $ "Invalid parameter to rst: " ++ show x



class InOut target source where
  in_ :: target -> source -> Z80ASM
  out :: source -> target -> Z80ASM

instance (Word8 ~ n) => InOut A [n] where
  in_ A [n] = code [0xdb, n]
  in_ _ x   = derefError x
  out [n] A = code [0xd3, n]
  out x   _ = derefError x

instance InOut Reg8 [C] where
  in_ r [C] = code [0xeb, 0x1 .<. 6 .|. encodeReg8 r .<. 3]
  in_ _ x   = derefError x
  out [C] r = code [0xed, 0x1 .<. 6 .|. encodeReg8 r .<. 3 .|. 0x1]
  out x   _ = derefError x
instance InOut A [C] where
  in_ r [C] = code [0xeb, 0x1 .<. 6 .|. encodeReg8 r .<. 3]
  in_ _ x   = derefError x
  out [C] r = code [0xed, 0x1 .<. 6 .|. encodeReg8 r .<. 3 .|. 0x1]
  out x   _ = derefError x
instance InOut C [C] where
  in_ r [C] = code [0xeb, 0x1 .<. 6 .|. encodeReg8 r .<. 3]
  in_ _ x   = derefError x
  out [C] r = code [0xed, 0x1 .<. 6 .|. encodeReg8 r .<. 3 .|. 0x1]
  out x   _ = derefError x

ini, inir, ind, indr :: Z80ASM
ini  = code [0xed, 0xa2]
inir = code [0xed, 0xb2]
ind  = code [0xed, 0xaa]
indr = code [0xed, 0xba]

outi, otir, outd, otdr :: Z80ASM
outi = code [0xed, 0xa3]
otir = code [0xed, 0xb3]
outd = code [0xed, 0xab]
otdr = code [0xed, 0xbb]

{- -------- INTERNAL UTILITIES -------- -}

(.<.) :: Bits a => a -> Int -> a
(.<.) = shiftL

hi, lo :: Word16 -> Word8
hi = fromIntegral . byteSwap16
lo = fromIntegral

-- Get an address relative to the current address
relative :: Word16 -> Z80 Word8
relative a = fromIntegral . validate . (`subtract` (fromIntegral a)) . asInt <$> label
  where asInt x = fromIntegral x :: Int
        validate x | x >= (-126) && x <= 129 = x
                   | otherwise = error $ "Relative address out of range: " ++ show x

derefError :: Show a => a -> b
derefError x = error $ "Dereference syntax is a list with exactly one entry. Invalid: " ++ show x

meaningless :: Monad m => String -> m a -> m b
meaningless s a = a >> return (error $ "Return value from " ++ s ++ " is meaningless")

class EncodeReg8 r where
  encodeReg8 :: r -> Word8

instance EncodeReg8 A where
  encodeReg8 A = 0x7 -- 111

instance EncodeReg8 C where
  encodeReg8 C = 0x1 -- 001

instance EncodeReg8 Reg8 where
  encodeReg8 B = 0x0 -- 000
  encodeReg8 D = 0x2 -- 010
  encodeReg8 E = 0x3 -- 011
  encodeReg8 H = 0x4 -- 100
  encodeReg8 L = 0x5 -- 101

class EncodeReg16 r where
  encodeReg16 :: r -> Word8

instance EncodeReg16 BC where
  encodeReg16 BC = 0x0 -- 00
instance EncodeReg16 DE where
  encodeReg16 DE = 0x1 -- 01
instance EncodeReg16 HL where
  encodeReg16 HL = 0x2 -- 10

-- SP and AF both encode to the same value.
-- This will break if there is ever a situation in which either could be passed.
instance EncodeReg16 SP where
  encodeReg16 SP = 0x3 -- 11
instance EncodeReg16 AF where
  encodeReg16 AF = 0x3 -- 11

class EncodeReg16 r => Reg16 r
instance Reg16 BC
instance Reg16 DE

instance EncodeReg16 RegIx where
  encodeReg16 IX = 0xdd
  encodeReg16 IY = 0xfd
  encodeReg16 i  = error $ "Cannot encode offset index: " ++ show i

encodeOrError :: EncodeReg16 x => Maybe x -> Word8
encodeOrError (Just x) = encodeReg16 x
encodeOrError Nothing  = error $ "Cannot encode register: no value"

class EncodeCondition r where
  encodeCondition :: r -> Word8

instance EncodeCondition Condition where
  encodeCondition PO = 0x4
  encodeCondition PE = 0x5
  encodeCondition P  = 0x6
  encodeCondition M  = 0x7

instance EncodeCondition NZ where
  encodeCondition NZ = 0x0
instance EncodeCondition Z where
  encodeCondition Z  = 0x1
instance EncodeCondition NC where
  encodeCondition NC = 0x2
instance EncodeCondition C where
  encodeCondition C  = 0x3

class EncodeCondition c => Cond c
instance Cond Condition
instance Cond NZ
instance Cond Z
instance Cond NC
instance Cond C
