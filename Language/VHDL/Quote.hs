{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.VHDL.Quote
-- Copyright   : (c) 2016-2017 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>
--
--------------------------------------------------------------------------------

module Language.VHDL.Quote (
    ToExp(..),
    vtype,
    vlit,
    vexp,
    vstm,
    vcstm,
    vdecl,
    videcl,
    vfile
  ) where

import Control.Monad ((>=>))
import Data.Data (Data(..))
import Data.Generics (extQ)
import Data.Loc
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import Data.Typeable (Typeable)
#ifdef FULL_HASKELL_ANTIQUOTES
import Language.Haskell.Meta (parseExp,
                              parsePat)
#else /* !defined(FULL_HASKELL_ANTIQUOTES) */
import Language.Haskell.ParseExp (parseExp,
                                  parsePat)
#endif /* !defined(FULL_HASKELL_ANTIQUOTES) */
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..),
                                  dataToExpQ,
                                  dataToPatQ)

import Language.VHDL.Parser
import qualified Language.VHDL.Parser.Parser as P
import qualified Language.VHDL.Syntax as V

defaultNamespace :: Map V.Name V.NameSpace
defaultNamespace = Map.fromList
  [ ("Word", V.TypeN)
  , ("Bit", V.TypeN)
  , ("Natural", V.TypeN)
  ]

-- | An instance of 'ToExp' can be converted to a 'V.Exp'.
class ToExp a where
    toExp :: a -> SrcLoc -> V.Exp

instance ToExp V.Exp where
    toExp e _ = e

vtype :: QuasiQuoter
vtype = quasiquote exts defaultNamespace P.parseType

vlit :: QuasiQuoter
vlit = quasiquote exts defaultNamespace P.parseLit

vexp :: QuasiQuoter
vexp = quasiquote exts defaultNamespace P.parseExp

vstm :: QuasiQuoter
vstm = quasiquote exts defaultNamespace P.parseStm

vcstm :: QuasiQuoter
vcstm = quasiquote exts defaultNamespace P.parseCStm

vdecl :: QuasiQuoter
vdecl = quasiquote exts defaultNamespace P.parseDecl

videcl :: QuasiQuoter
videcl = quasiquote exts defaultNamespace P.parseIDecl

vfile :: QuasiQuoter
vfile = quasiquote exts defaultNamespace P.parseDesignFile

quasiquote :: Data a
           => [V.Extension]
           -> Map V.Name V.NameSpace
           -> P a
           -> QuasiQuoter
quasiquote exts ns p =
    QuasiQuoter { quoteExp  = qqparse exts ns p >=> dataToExpQ qqExp
                , quotePat  = qqparse exts ns p >=> dataToPatQ qqPat
                , quoteType = fail "VHDL type quasiquoter undefined"
                , quoteDec  = fail "VHDL declaration quasiquoter undefined"
                }
  where
    qqparse :: [V.Extension]
            -> Map V.Name V.NameSpace
            -> P a
            -> String
            -> Q a
    qqparse exts ns p s = do
        loc <- location
        case parse (V.Antiquotation : exts) ns p (T.pack s) (Just (locToPos loc)) of
          Left err -> fail (show err)
          Right x  -> return x
      where
        locToPos :: Language.Haskell.TH.Loc -> Pos
        locToPos loc = Pos (loc_filename loc)
                           ((fst . loc_start) loc)
                           ((snd . loc_start) loc)
                           0

qqExp :: Typeable a => a -> Maybe ExpQ
qqExp = const Nothing `extQ` qqExpE

qqPat :: Typeable a => a -> Maybe PatQ
qqPat = const Nothing `extQ` qqExpP

exts :: [V.Extension]
exts = []

antiExpQ :: String -> ExpQ
antiExpQ = either fail return . parseExp

antiPatQ :: String -> PatQ
antiPatQ = either fail return . parsePat

qqLocE :: SrcLoc -> ExpQ
qqLocE = dataToExpQ qqExp

qqExpE :: V.Exp -> Maybe ExpQ
qqExpE (V.AntiExp e loc)    = Just [|toExp $(antiExpQ e) $(qqLocE loc) :: V.Exp|]
qqExpE _                    = Nothing

qqExpP :: V.Exp -> Maybe PatQ
qqExpP (V.AntiExp e _)    = Just $ antiPatQ e
qqExpP _                  = Nothing
