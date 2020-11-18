{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
    ToLit(..),
    ToId(..),
    ToExp(..),
    ToType(..),
    vtype,
    vlit,
    vexp,
    vstm,
    vcstm,
    vdecl,
    videcl,
    vassocs,
    vunit,
    vfile
  ) where

import Control.Monad ((>=>))
import Data.Data (Data(..))
import Data.Generics (extQ)
import Data.Loc
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Symbol ( intern )
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

-- | An instance of 'ToLit' can be converted to a 'V.Lit'.
class ToLit a where
    toLit :: a -> SrcLoc -> V.Lit

instance ToLit V.Lit where
    toLit l _ = l

instance ToLit Integer where
    toLit n loc = V.IntLit (show n) n loc

instance ToLit Int where
    toLit n loc = V.IntLit (show n) (toInteger n) loc

instance ToLit Rational where
    toLit n loc = V.RealLit (show n) n loc

instance ToLit Float where
    toLit n loc = V.RealLit (show n) (toRational n) loc

instance ToLit Double where
    toLit n loc = V.RealLit (show n) (toRational n) loc

-- | An instance of 'ToId' can be converted to a 'V.Id'.
class ToId a where
    toId :: a -> SrcLoc -> V.Id

instance ToId V.Id where
    toId ident _ = ident

instance ToId String where
    toId ident l = V.Id (V.mkNoCase (intern ident)) l

-- | An instance of 'ToExp' can be converted to a 'V.Exp'.
class ToExp a where
    toExp :: a -> SrcLoc -> V.Exp

instance ToExp V.Exp where
    toExp e _ = e

instance ToExp Integer where
    toExp n loc = V.LitE (toLit n loc) loc

instance ToExp Int where
    toExp n loc = V.LitE (toLit n loc) loc

instance ToExp Rational where
    toExp n loc = V.LitE (toLit n loc) loc

instance ToExp Float where
    toExp n loc = V.LitE (toLit n loc) loc

instance ToExp Double where
    toExp n loc = V.LitE (toLit n loc) loc

-- | An instance of 'ToType' can be converted to a 'V.Subtype'.
class ToType a where
    toType :: a -> SrcLoc -> V.Subtype

instance ToType V.Subtype where
    toType tau _ = tau

qq :: Data a => P a -> QuasiQuoter
qq = quasiquote defaultExtensions defaultTypes
  where
    defaultExtensions :: [V.Extension]
    defaultExtensions = []

    defaultTypes :: Set V.Id
    defaultTypes = Set.fromList
      [ "Word"
      , "Bit"
      , "Natural"
      , "std_logic"
      , "std_logic_vector"
      , "sfixed"
      , "ufixed"
      ]

vtype :: QuasiQuoter
vtype = qq P.parseType

vlit :: QuasiQuoter
vlit = qq P.parseLit

vexp :: QuasiQuoter
vexp = qq P.parseExp

vassocs :: QuasiQuoter
vassocs = qq P.parseAssocs

vstm :: QuasiQuoter
vstm = qq P.parseStm

vcstm :: QuasiQuoter
vcstm = qq P.parseCStm

vdecl :: QuasiQuoter
vdecl = qq P.parseDecl

videcl :: QuasiQuoter
videcl = qq P.parseIDecl

vunit :: QuasiQuoter
vunit = qq P.parseDesignUnit

vfile :: QuasiQuoter
vfile = qq P.parseDesignFile

quasiquote :: Data a
           => [V.Extension]
           -> Set V.Id
           -> P a
           -> QuasiQuoter
quasiquote exts0 types0 p0 =
    QuasiQuoter { quoteExp  = qqparse exts0 types0 p0 >=> dataToExpQ qqExp
                , quotePat  = qqparse exts0 types0 p0 >=> dataToPatQ qqPat
                , quoteType = error "VHDL type quasiquoter undefined"
                , quoteDec  = error "VHDL declaration quasiquoter undefined"
                }
  where
    qqparse :: [V.Extension]
            -> Set V.Id
            -> P a
            -> String
            -> Q a
    qqparse exts types p s = do
        loc <- location
        case parse (V.Antiquotation : exts) types p (T.pack s) (Just (locToPos loc)) of
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
qqLitE (V.AntiLit e loc)  = Just [|toLit $(antiExpQ e) $(qqLocE loc) :: V.Lit|]
qqLitE _                  = Nothing

qqIdE :: V.Id -> Maybe ExpQ
qqIdE (V.AntiId e loc) = Just [|toId $(antiExpQ e) $(qqLocE loc) :: V.Id|]
qqIdE _                = Nothing

qqExpE :: V.Exp -> Maybe ExpQ
qqExpE (V.AntiExp e loc)    = Just [|toExp $(antiExpQ e) $(qqLocE loc) :: V.Exp|]
qqExpE _                    = Nothing

qqStmE :: V.Stm -> Maybe ExpQ
qqStmE (V.AntiStm stm _) = Just $ antiExpQ stm
qqStmE _                 = Nothing

qqStmsE :: [V.Stm] -> Maybe ExpQ
qqStmsE []                      = Just [|[]|]
qqStmsE (V.AntiStms v _ : stms) = Just [|$(antiExpQ v) ++ $(dataToExpQ qqExp stms)|]
qqStmsE (stm : stms)            = Just [|$(dataToExpQ qqExp stm) : $(dataToExpQ qqExp stms)|]

qqCStmE :: V.CStm -> Maybe ExpQ
qqCStmE (V.AntiCStm stm _) = Just $ antiExpQ stm
qqCStmE _                  = Nothing

qqCStmsE :: [V.CStm] -> Maybe ExpQ
qqCStmsE []                       = Just [|[]|]
qqCStmsE (V.AntiCStms v _ : stms) = Just [|$(antiExpQ v) ++ $(dataToExpQ qqExp stms)|]
qqCStmsE (stm : stms)            = Just [|$(dataToExpQ qqExp stm) : $(dataToExpQ qqExp stms)|]


qqSubtypeE :: V.Subtype -> Maybe ExpQ
qqSubtypeE (V.AntiType e loc) = Just [|toType $(antiExpQ e) $(qqLocE loc) :: V.Subtype|]
qqSubtypeE _                  = Nothing

qqElemAssocListE:: [V.ElemAssoc] -> Maybe (Q Exp)
qqElemAssocListE [] =
    Just [|[]|]
qqElemAssocListE (V.AntiExpsElemAssoc v loc : inits) =
    Just [|[V.ElemAssoc [] (toExp e $(qqLocE loc)) $(qqLocE loc) | e <- $(antiExpQ v)] ++ $(dataToExpQ qqExp inits)|]
qqElemAssocListE (V.AntiLitsElemAssoc v loc : inits) =
    Just [|[V.ElemAssoc [] (V.LitE (toLit e $(qqLocE loc)) $(qqLocE loc)) $(qqLocE loc) | e <- $(antiExpQ v)] ++ $(dataToExpQ qqExp inits)|]
qqElemAssocListE (ini : inis) =
    Just [|$(dataToExpQ qqExp ini) : $(dataToExpQ qqExp inis)|]

qqExp :: Typeable a => a -> Maybe ExpQ
qqExp = const Nothing `extQ` qqStringE
                      `extQ` qqLitE
                      `extQ` qqIdE
                      `extQ` qqExpE
                      `extQ` qqStmE
                      `extQ` qqStmsE
                      `extQ` qqCStmE
                      `extQ` qqCStmsE
                      `extQ` qqSubtypeE
                      `extQ` qqElemAssocListE

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

qqIdP :: V.Id -> Maybe PatQ
qqIdP (V.AntiId e _) = Just $ antiPatQ e
qqIdP _              = Nothing

qqExpP :: V.Exp -> Maybe PatQ
qqExpP (V.AntiExp e _)    = Just $ antiPatQ e
qqExpP _                  = Nothing

qqPat :: Typeable a => a -> Maybe PatQ
qqPat = const Nothing `extQ` qqStringP
                      `extQ` qqLocP
                      `extQ` qqLitP
                      `extQ` qqIdP
                      `extQ` qqExpP
