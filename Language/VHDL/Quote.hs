{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.VHDL.Quote
-- Copyright   : (c) 2016-2020 Drexel University
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

-- | An instance of 'ToExp' can be converted to a 'V.Exp'.
class ToExp a where
    toExp :: a -> SrcLoc -> V.Exp

instance ToExp V.Exp where
    toExp e _ = e

qq :: Data a => P a -> QuasiQuoter
qq = quasiquote defaultExtensions defaultNamespace
  where
    defaultExtensions :: [V.Extension]
    defaultExtensions = []

    defaultNamespace :: Map V.Name V.NameSpace
    defaultNamespace = Map.fromList
      [ ("Word", V.TypeN)
      , ("Bit", V.TypeN)
      , ("Natural", V.TypeN)
      ]

vtype :: QuasiQuoter
vtype = qq P.parseType

vlit :: QuasiQuoter
vlit = qq P.parseLit

vexp :: QuasiQuoter
vexp = qq P.parseExp

vstm :: QuasiQuoter
vstm = qq P.parseStm

vcstm :: QuasiQuoter
vcstm = qq P.parseCStm

vdecl :: QuasiQuoter
vdecl = qq P.parseDecl

videcl :: QuasiQuoter
videcl = qq P.parseIDecl

vfile :: QuasiQuoter
vfile = qq P.parseDesignFile

quasiquote :: Data a
           => [V.Extension]
           -> Map V.Name V.NameSpace
           -> P a
           -> QuasiQuoter
quasiquote exts0 ns0 p0 =
    QuasiQuoter { quoteExp  = qqparse exts0 ns0 p0 >=> dataToExpQ qqExp
                , quotePat  = qqparse exts0 ns0 p0 >=> dataToPatQ qqPat
                , quoteType = error "VHDL type quasiquoter undefined"
                , quoteDec  = error "VHDL declaration quasiquoter undefined"
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

antiExpQ :: String -> ExpQ
antiExpQ = either fail return . parseExp

qqLocE :: SrcLoc -> ExpQ
qqLocE = dataToExpQ qqExp

qqStringE :: String -> Maybe (Q Exp)
qqStringE s = Just $ litE $ stringL s

qqLitE :: V.Lit -> Maybe ExpQ
qqLitE (V.AntiInt e loc)  = Just [|let x = $(antiExpQ e)
                                   in
                                     V.IntLit (show x)
                                              (fromIntegral x)
                                              $(qqLocE loc)|]
qqLitE (V.AntiReal e loc) = Just [|let x = $(antiExpQ e)
                                   in
                                     V.RealLit (show x)
                                               (toRational x)
                                               $(qqLocE loc)|]
qqLitE _                  = Nothing

qqExpE :: V.Exp -> Maybe ExpQ
qqExpE (V.AntiExp e loc)    = Just [|toExp $(antiExpQ e) $(qqLocE loc) :: V.Exp|]
qqExpE _                    = Nothing

qqExp :: Typeable a => a -> Maybe ExpQ
qqExp = const Nothing `extQ` qqStringE
                      `extQ` qqLitE
                      `extQ` qqExpE

antiPatQ :: String -> PatQ
antiPatQ = either fail return . parsePat

qqStringP :: String -> Maybe (Q Pat)
qqStringP s = Just $ litP $ stringL s

qqLocP :: Data.Loc.Loc -> Maybe (Q Pat)
qqLocP _ = Just wildP

qqLitP :: V.Lit -> Maybe PatQ
qqLitP = go
  where
    go (V.AntiInt e _)  = Just $ con "V.IntLit" [wildP, antiPatQ e, wildP]
    go (V.AntiReal e _) = Just $ con "V.RealLit" [wildP, antiPatQ e, wildP]
    go _                = Nothing

    con n = conP (mkName n)

qqExpP :: V.Exp -> Maybe PatQ
qqExpP (V.AntiExp e _)    = Just $ antiPatQ e
qqExpP _                  = Nothing

qqPat :: Typeable a => a -> Maybe PatQ
qqPat = const Nothing `extQ` qqStringP
                      `extQ` qqLocP
                      `extQ` qqLitP
                      `extQ` qqExpP
