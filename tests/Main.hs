{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) 2016-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main (
    main
  ) where

import Test.HUnit (Assertion, (@?=))
import Test.Hspec

import Language.VHDL.Quote
import Language.VHDL.Syntax

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    decimalLiteralTests
    basedLiteralTests
    expressionTests

decimalLiteralTests :: Spec
decimalLiteralTests =
    describe "Decimal literal tests" $ do
      it "Int constant 12" $ isIntLit [vlit|12|] 12
      it "Int constant 0" $ isIntLit [vlit|0|] 0
      it "Int constant 1e6" $ isIntLit [vlit|1e6|] 1000000
      it "Real constant 12.0" $ isRealLit [vlit|12.0|] 12
      it "Real constant 0.0" $ isRealLit [vlit|0.0|] 0
      it "Real constant 0.456" $ isRealLit [vlit|0.456|] 0.456
      it "Real constant 3.14159_26" $ isRealLit [vlit|3.14159_26|] 3.1415926
      it "Real constant 1.34E-12" $ isRealLit [vlit|1.34E-12|] 1.34E-12
      it "Real constant 1.0E+6" $ isRealLit [vlit|1.0E+6|] 1.0E+6
      it "Real constant 6.023E+24" $ isRealLit [vlit|6.023E+24|] 6.023E+24

basedLiteralTests :: Spec
basedLiteralTests =
    describe "Based literal tests" $ do
      it "Int constant 2#1111_1111#" $ isIntLit [vlit|2#1111_1111#|] 255
      it "Int constant 16#FF#" $ isIntLit [vlit|16#FF#|] 255
      it "Int constant 016#0FF#" $ isIntLit [vlit|016#0FF#|] 255
      it "Int constant 16#E#E1" $ isIntLit [vlit|16#E#E1|] 224
      it "Int constant 2#1110_0000#" $ isIntLit [vlit|2#1110_0000#|] 224
      it "Real constant 16#F.FF#E+2" $ isRealLit [vlit|16#F.FF#E+2|] 4095.0
      it "Real constant 2#1.1111_1111_111#E11" $ isRealLit [vlit|2#1.1111_1111_111#E11|] 4095.0

expressionTests :: Spec
expressionTests =
    describe "Expression antiquote tests" $ do
      it "addition expression" $ [vexp|1 + 2|] @?= [vexp|$one + 2|]
      it "and expression" $ [vexp|and 1|] @?= [vexp|and $one|]
      it "declaration" $
        let d1 = [vdecl|entity ROM is
                        port (Addr: in Word;
                              Data: out Word;
                              Sel: in Bit);
                        type Instruction is array (1 to 5) of Natural;
                        type Program is array (Natural range <>) of Instruction;
                        use Work.OpCodes.all, Work.RegisterNames.all;
                        constant ROM_Code: Program :=
                          (
                            (STM, R14, R12, 12, R13),
                            (LD, R7, 32, 0, R1 ),
                            (BAL, R14, 0, 0, R7 )
                          ) ;
                        end ROM;|]
            d2 = [vdecl|entity ROM is
                        port (Addr: in Word;
                              Data: out Word;
                              Sel: in Bit);
                        type Instruction is array ($one to 5) of Natural;
                        type Program is array (Natural range <>) of Instruction;
                        use Work.OpCodes.all, Work.RegisterNames.all;
                        constant ROM_Code: Program :=
                          (
                            (STM, R14, R12, 12, R13),
                            (LD, R7, 32, 0, R1 ),
                            (BAL, R14, 0, 0, R7 )
                          ) ;
                        end ROM;|]
          in
            d1 @?= d2
  where
    one :: Exp
    one = [vexp|1|]

isIntLit :: Lit -> Integer -> Assertion
isIntLit (IntLit _ i _) i' = i @?= i'
isIntLit _              _  = fail "Not an integer literal"

isRealLit :: Lit -> Rational -> Assertion
isRealLit (RealLit _ i _) i' = i @?= i'
isRealLit _               _  = fail "Not a rational literal"