{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Ex where

import Z80
import qualified Data.ByteString as BS

ex :: IO ()
ex = BS.writeFile "test.bin" . asmData . org 0x6000 $ mdo
  ld a 2                 -- upper screen
  call 5633              -- open channel

  withLabel $ \loop -> do
    ld de string         -- address of string
    ld bc (eostr-string) -- length of string to print
    call 8252            -- print our string
    jp loop              -- repeat until screen is full

  string <- labelled $ db "Dani is cool"
  eostr  <- label

  return ()
