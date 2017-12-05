{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main (
    main
  ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, (@?=))

import Language.VHDL.Quote
import Language.VHDL.Syntax

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ decimalLiteralTests
        , basedLiteralTests
        , expressionTests
        ]

decimalLiteralTests :: Test
decimalLiteralTests = testGroup "15.5.2 Decimal literals"
    [ testCase "Int constant 12" (isIntLit [vlit|12|] 12)
    , testCase "Int constant 0" (isIntLit [vlit|0|] 0)
    , testCase "Int constant 1e6" (isIntLit [vlit|1e6|] 1000000)
    , testCase "Real constant 12.0" (isRealLit [vlit|12.0|] 12)
    , testCase "Real constant 0.0" (isRealLit [vlit|0.0|] 0)
    , testCase "Real constant 0.456" (isRealLit [vlit|0.456|] 0.456)
    , testCase "Real constant 3.14159_26" (isRealLit [vlit|3.14159_26|] 3.1415926)
    , testCase "Real constant 1.34E-12" (isRealLit [vlit|1.34E-12|] 1.34E-12)
    , testCase "Real constant 1.0E+6" (isRealLit [vlit|1.0E+6|] 1.0E+6)
    , testCase "Real constant 6.023E+24" (isRealLit [vlit|6.023E+24|] 6.023E+24)
    ]

basedLiteralTests :: Test
basedLiteralTests = testGroup "15.5.3 Based literals"
    [ testCase "Int constant 2#1111_1111#" (isIntLit [vlit|2#1111_1111#|] 255)
    , testCase "Int constant 16#FF#" (isIntLit [vlit|16#FF#|] 255)
    , testCase "Int constant 016#0FF#" (isIntLit [vlit|016#0FF#|] 255)
    , testCase "Int constant 16#E#E1" (isIntLit [vlit|16#E#E1|] 224)
    , testCase "Int constant 2#1110_0000#" (isIntLit [vlit|2#1110_0000#|] 224)
    , testCase "Real constant 16#F.FF#E+2" (isRealLit [vlit|16#F.FF#E+2|] 4095.0)
    , testCase "Real constant 2#1.1111_1111_111#E11" (isRealLit [vlit|2#1.1111_1111_111#E11|] 4095.0)
    ]

isIntLit :: Lit -> Integer -> Assertion
isIntLit (IntLit _ i _) i' = i @?= i'
isIntLit _              _  = fail "Not an integer literal"

isRealLit :: Lit -> Rational -> Assertion
isRealLit (RealLit _ i _) i' = i @?= i'
isRealLit _               _  = fail "Not a rational literal"

expressionTests :: Test
expressionTests = testGroup "Expressions"
    [ t1
    , t2
    ]
  where
    t1 :: Test
    t1 = testCase "t1" $
         [vexp|1 + 2|] @?= [vexp|$one + 2|]

    t2 :: Test
    t2 = testCase "t2" $
         [vexp|and 1|] @?= [vexp|and $one|]

    one :: Exp
    one = [vexp|1|]
