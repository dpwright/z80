{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Lower-case versions of the Z80 registers/operands.
-- The original Z80 assembler supported this, but I find it useful
-- to have operands and labels in different namespaces, so I've put
-- lower-case equivalents into a separate module so you can choose
-- whether or not to make use of them.

module Z80.Operands.LowerCase
  ( a, b, c, d, e, f, h, l, i, r
  , bc, de, hl, af, af', sp, pc, ix, iy
  , z, nz, nc, po, pe, p, m
  ) where

import Z80.Operands

a   = A
b   = B
c   = C
d   = D
e   = E
f   = F
h   = H
l   = L
i   = I
r   = R
bc  = BC
de  = DE
hl  = HL
af  = AF
af' = AF'
sp  = SP
pc  = PC
ix  = IX
iy  = IY
z   = Z
nz  = NZ
nc  = NC
po  = PO
pe  = PE
p   = P
m   = M
