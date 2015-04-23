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
  testGroup "Load Group"
  [ testCase "LD r, r'"     $ ld H E               $?= [0x63]
  , testCase "LD r, n"      $ ld E 0xa5            $?= [0x1e, 0xa5]
  , testCase "LD r, (HL)"   $ ld C [HL]            $?= [0x4e]
  , testCase "LD r, (IX+d)" $ ld B [IX :+ 0x19]    $?= [0xdd, 0x46, 0x19]
  , testCase "LD r, (IY+d)" $ ld B [IY :+ 0x19]    $?= [0xfd, 0x46, 0x19]
  , testCase "LD (HL), r"   $ ld [HL] B            $?= [0x70]
  , testCase "LD (IX+d), r" $ ld [IX :+ 0x6] C     $?= [0xdd, 0x71, 0x06]
  , testCase "LD (IY+d), r" $ ld [IY :+ 0x4] C     $?= [0xfd, 0x71, 0x04]
  , testCase "LD (HL), n"   $ ld [HL] 0x28         $?= [0x36, 0x28]
  , testCase "LD (IX+d), n" $ ld [IX :+ 0x5] 0x5a  $?= [0xdd, 0x36, 0x05, 0x5a]
  , testCase "LD (IY+d), n" $ ld [IY :+ 0x10] 0x97 $?= [0xfd, 0x36, 0x10, 0x97]
  , testCase "LD A, (BC)"   $ ld A [BC]            $?= [0x0a]
  , testCase "LD A, (DE)"   $ ld A [DE]            $?= [0x1a]
  , testCase "LD A, (nn)"   $ ld A [0x8832]        $?= [0x3a, 0x32, 0x88]
  , testCase "LD (BC), A"   $ ld [BC] A            $?= [0x02]
  , testCase "LD (DE), A"   $ ld [DE] A            $?= [0x12]
  , testCase "LD (nn), A"   $ ld [0x3141] A        $?= [0x32, 0x41, 0x31]
  , testCase "LD A, I"      $ ld A I               $?= [0xed, 0x57]
  , testCase "LD A, R"      $ ld A R               $?= [0xed, 0x5f]
  , testCase "LD I, A"      $ ld I A               $?= [0xed, 0x47]
  , testCase "LD R, A"      $ ld R A               $?= [0xed, 0x4f]
  , testCase "LD dd, nn"    $ ld HL 0x5000         $?= [0x21, 0x00, 0x50]
  , testCase "LD IX, nn"    $ ld IX 0x45A2         $?= [0xdd, 0x21, 0xA2, 0x45]
  , testCase "LD IY, nn"    $ ld IY 0x7733         $?= [0xfd, 0x21, 0x33, 0x77]
  , testCase "LD HL, (nn)"  $ ld HL [0x4545]       $?= [0x2a, 0x45, 0x45]
  , testCase "LD dd, (nn)"  $ ld BC [0x2130]       $?= [0xed, 0x4b, 0x30, 0x21]
  , testCase "LD IX, (nn)"  $ ld IX [0x6666]       $?= [0xdd, 0x2a, 0x66, 0x66]
  , testCase "LD IY, (nn)"  $ ld IY [0x6666]       $?= [0xfd, 0x2a, 0x66, 0x66]
  , testCase "LD (nn), HL"  $ ld [0xb229] HL       $?= [0x22, 0x29, 0xb2]
  , testCase "LD (nn), dd"  $ ld [0x1000] BC       $?= [0xed, 0x43, 0x00, 0x10]
  , testCase "LD (nn), IX"  $ ld [0x4392] IX       $?= [0xdd, 0x22, 0x92, 0x43]
  , testCase "LD (nn), IY"  $ ld [0x8838] IY       $?= [0xfd, 0x22, 0x38, 0x88]
  , testCase "LD SP, HL"    $ ld SP HL             $?= [0xf9]
  , testCase "LD SP, IX"    $ ld SP IX             $?= [0xdd, 0xf9]
  , testCase "LD SP, IY"    $ ld SP IY             $?= [0xfd, 0xf9]
  , testCase "PUSH qq"      $ push AF              $?= [0xf5]
  , testCase "PUSH IX"      $ push IX              $?= [0xdd, 0xe5]
  , testCase "PUSH IY"      $ push IY              $?= [0xfd, 0xe5]
  , testCase "POP qq"       $ pop HL               $?= [0xe1]
  , testCase "POP IX"       $ pop IX               $?= [0xdd, 0xe1]
  , testCase "POP IY"       $ pop IY               $?= [0xfd, 0xe1]
  ]
  ]
