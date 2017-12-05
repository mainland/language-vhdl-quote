{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main (
    main
  ) where

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Language.VHDL.Quote
import Language.VHDL.Syntax

main :: IO ()
main = do
    putDocLn $ ppr d1
  where
    d1 :: Decl
    d1 = [vdecl|entity ROM is
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
