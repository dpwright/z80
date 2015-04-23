import Test.Tasty
import Test.Tasty.HUnit

import Data.ByteString
import Data.Word

import Z80

($?=) :: Z80ASM -> [Word8] -> Assertion
asm $?= bytes = extractBytes asm @?= pack bytes
  where extractBytes = asmData . org 0

main :: IO ()
main = defaultMain $ testGroup "Tests" [
  ]
