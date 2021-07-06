{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.VHDL.Quote
-- Copyright   : (c) 2016-2021 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>
--
--------------------------------------------------------------------------------

module Language.VHDL.Quote (
    ToLit(..),
    bitsL,
    ToId(..),
    ToName(..),
    ToExp(..),
    ToType(..),
    vtype,
    vname,
    vlit,
    vexp,
    vcondexp,
    vcondwave,
    vstm,
    vstms,
    vcstm,
    vcstms,
    vdecl,
    videcl,
    vassoc,
    vassocs,
    vrange,
    vunit,
    vfile
  ) where

import Control.Monad ((>=>))
import Data.Bits
import Data.Data (Data(..))
import Data.Generics (extQ)
import Data.Loc
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Symbol ( intern )
import Data.Symbol.Unsafe ( Symbol(..) )
import qualified Data.Text.Lazy as T
import Data.Typeable (Typeable)
#ifdef FULL_HASKELL_ANTIQUOTES
import Language.Haskell.Exts.Extension (Extension(..),
                                        KnownExtension(..),
                                        Language(..))
import Language.Haskell.Exts.Parser (ParseMode(..),
                                     defaultParseMode,
                                     parseExpWithMode,
                                     parsePatWithMode)
import qualified Language.Haskell.Exts.SrcLoc as Hs
import qualified Language.Haskell.Exts.Syntax as Hs
import Language.Haskell.Meta (parseResultToEither)
import qualified Language.Haskell.Meta as Meta
#else /* !defined(FULL_HASKELL_ANTIQUOTES) */
import Language.Haskell.ParseExp (parseExp,
                                  parsePat)
#endif /* !defined(FULL_HASKELL_ANTIQUOTES) */
import Language.Haskell.TH hiding (Extension(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..),
                                  dataToExpQ,
                                  dataToPatQ)

import Language.VHDL.Parser
import qualified Language.VHDL.Parser.Parser as P
import qualified Language.VHDL.Syntax as V

#ifdef FULL_HASKELL_ANTIQUOTES
parseMode :: ParseMode
parseMode = defaultParseMode
  { parseFilename = []
  , baseLanguage = Haskell2010
  , extensions = map EnableExtension defaultExtensions :: [Extension]
  }
  where
    defaultExtensions :: [KnownExtension]
    defaultExtensions = [ DataKinds
                        , ForeignFunctionInterface
                        , MagicHash
                        , MultiParamTypeClasses
                        , PatternSignatures
                        , PostfixOperators
                        , RankNTypes
                        , RecursiveDo
                        , QuasiQuotes
                        , TemplateHaskell
                        , TypeApplications
                        , UnicodeSyntax
                        ]

parsePat :: String -> Either String Pat
parsePat = either Left (Right . Meta.toPat) . parseHsPat

parseExp :: String -> Either String Exp
parseExp = either Left (Right . Meta.toExp) . parseHsExp

parseHsExp :: String -> Either String (Hs.Exp Hs.SrcSpanInfo)
parseHsExp = parseResultToEither . parseExpWithMode parseMode

parseHsPat :: String -> Either String (Hs.Pat Hs.SrcSpanInfo)
parseHsPat = parseResultToEither . parsePatWithMode parseMode
#endif /* !defined(FULL_HASKELL_ANTIQUOTES) */

-- | An instance of 'ToLit' can be converted to a 'V.Lit'.
class ToLit a where
    toLit :: a -> SrcLoc -> V.Lit

-- | Compute the VHDL bit string literal representation of a value that is a
-- member of @FiniteBits@
bitsL :: FiniteBits a => a -> SrcLoc -> V.Lit
bitsL x l = V.BitStringLit (quote bs) l
  where
    bs = [if testBit x i then '1' else '0' | i <- [n-1, n-2..0]]
    n = finiteBitSize x

    quote s = '\"' : s ++ "\""

instance ToLit V.Lit where
    toLit l _ = l

instance ToLit Bool where
    toLit x l = V.BoolLit x l

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

instance ToLit String where
    toLit s loc = V.StringLit (show s) s loc

-- | An instance of 'ToId' can be converted to a 'V.Id'.
class ToId a where
    toId :: a -> SrcLoc -> V.Id

instance ToId V.Id where
    toId ident _ = ident

instance ToId String where
    toId s l = V.Id (V.mkNoCase (intern s)) l

-- | An instance of 'ToName' can be converted to a 'V.Name'.
class ToName a where
    toName :: a -> SrcLoc -> V.Name

instance ToName V.Name where
    toName n _ = n

instance ToName V.Id where
    toName ident l = V.SimpleN [] ident l

instance ToName String where
    toName s l = V.SimpleN [] (toId s l) l

-- | An instance of 'ToExp' can be converted to a 'V.Exp'.
class ToExp a where
    toExp :: a -> SrcLoc -> V.Exp

instance ToExp V.Id where
    toExp n l = V.VarE (toName n l) l

instance ToExp V.Name where
    toExp n l = V.VarE n l

instance ToExp V.Lit where
    toExp lit l = V.LitE lit l

instance ToExp V.Exp where
    toExp e _ = e

instance ToExp Bool where
    toExp n loc = V.LitE (toLit n loc) loc

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

instance ToExp String where
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
      [ -- Predefined types
        "bit"
      , "boolean"
      , "character"
      , "integer"
      , "natural"
      , "positive"
      , "real"
      , "string"
      , "time"
      , "bit_vector"
      , "boolean_vector"
      , "integer_vector"
      , "real_vector"
      , "time_vector"

        -- Text types
      , "text"
      , "line"

        -- IEEE types
      , "signed"
      , "unsigned"
      , "std_logic"
      , "std_logic_vector"
      , "std_ulogic"
      , "std_ulogic_vector"

        -- IEEE types fixed point types
      , "sfixed"
      , "ufixed"
      ]

vtype :: QuasiQuoter
vtype = qq P.parseType

vname :: QuasiQuoter
vname = qq P.parseName

vlit :: QuasiQuoter
vlit = qq P.parseLit

vexp :: QuasiQuoter
vexp = qq P.parseExp

vcondexp :: QuasiQuoter
vcondexp = qq P.parseCondExp

vcondwave :: QuasiQuoter
vcondwave = qq P.parseCondWave

vassoc :: QuasiQuoter
vassoc = qq P.parseAssoc

vassocs :: QuasiQuoter
vassocs = qq P.parseAssocs

vrange :: QuasiQuoter
vrange = qq P.parseDiscreteRange

vstm :: QuasiQuoter
vstm = qq P.parseStm

vstms :: QuasiQuoter
vstms = qq P.parseStms

vcstm :: QuasiQuoter
vcstm = qq P.parseCStm

vcstms :: QuasiQuoter
vcstms = qq P.parseCStms

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

qqSymbolE :: Symbol -> Maybe (Q Exp)
qqSymbolE (Symbol _ s) = Just [|intern $(litE (stringL s))|]

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

qqNameE :: V.Name -> Maybe ExpQ
qqNameE (V.AntiName v loc) = Just [|toName $(antiExpQ v) $(qqLocE loc) :: V.Name|]
qqNameE _                  = Nothing

qqNamesE :: [V.Name] -> Maybe ExpQ
qqNamesE []                     = Just [|[]|]
qqNamesE (V.AntiNames v _ : ns) = Just [|$(antiExpQ v) ++ $(dataToExpQ qqExp ns)|]
qqNamesE (n : ns)               = Just [|$(dataToExpQ qqExp n) : $(dataToExpQ qqExp ns)|]

qqExpE :: V.Exp -> Maybe ExpQ
qqExpE (V.AntiExp e loc)    = Just [|toExp $(antiExpQ e) $(qqLocE loc) :: V.Exp|]
qqExpE _                    = Nothing

qqCondE :: V.Conditional V.Exp -> Maybe ExpQ
qqCondE (V.AntiCond e _) = Just [|$(antiExpQ e) :: V.Conditional V.Exp|]
qqCondE _                = Nothing

qqCondWaveE :: V.Conditional V.Waveform -> Maybe ExpQ
qqCondWaveE (V.AntiCond e _) = Just [|$(antiExpQ e) :: V.Conditional V.Waveform|]
qqCondWaveE _                = Nothing

qqAssocElemE :: V.AssocElem -> Maybe ExpQ
qqAssocElemE (V.AntiAssocElem e loc) = Just [|$(antiExpQ e) $(qqLocE loc) :: V.AssocElem|]
qqAssocElemE _                       = Nothing

qqAssocElemsE :: [V.AssocElem] -> Maybe ExpQ
qqAssocElemsE []                          = Just [|[]|]
qqAssocElemsE (V.AntiAssocElems e _ : es) = Just [|$(antiExpQ e) ++ $(dataToExpQ qqExp es)|]
qqAssocElemsE (e : es)                    = Just [|$(dataToExpQ qqExp e) : $(dataToExpQ qqExp es)|]

qqDeclE :: V.Decl -> Maybe ExpQ
qqDeclE (V.AntiDecl decl _) = Just $ antiExpQ decl
qqDeclE _                   = Nothing

qqDeclsE :: [V.Decl] -> Maybe ExpQ
qqDeclsE []                        = Just [|[]|]
qqDeclsE (V.AntiDecls v _ : decls) = Just [|$(antiExpQ v) ++ $(dataToExpQ qqExp decls)|]
qqDeclsE (decl : decls)            = Just [|$(dataToExpQ qqExp decl) : $(dataToExpQ qqExp decls)|]

qqIDeclE :: V.IDecl -> Maybe ExpQ
qqIDeclE (V.AntiIDecl idecl _) = Just $ antiExpQ idecl
qqIDeclE _                     = Nothing

qqIDeclsE :: [V.IDecl] -> Maybe ExpQ
qqIDeclsE []                          = Just [|[]|]
qqIDeclsE (V.AntiIDecls v _ : idecls) = Just [|$(antiExpQ v) ++ $(dataToExpQ qqExp idecls)|]
qqIDeclsE (idecl : idecls)            = Just [|$(dataToExpQ qqExp idecl) : $(dataToExpQ qqExp idecls)|]

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
qqCStmsE (stm : stms)             = Just [|$(dataToExpQ qqExp stm) : $(dataToExpQ qqExp stms)|]

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

qqRangeE :: V.DiscreteRange -> Maybe ExpQ
qqRangeE (V.AntiRange rng _) = Just $ antiExpQ rng
qqRangeE _                   = Nothing

qqExp :: Typeable a => a -> Maybe ExpQ
qqExp = const Nothing `extQ` qqStringE
                      `extQ` qqSymbolE
                      `extQ` qqLitE
                      `extQ` qqIdE
                      `extQ` qqNameE
                      `extQ` qqNamesE
                      `extQ` qqExpE
                      `extQ` qqCondE
                      `extQ` qqCondWaveE
                      `extQ` qqAssocElemE
                      `extQ` qqAssocElemsE
                      `extQ` qqDeclE
                      `extQ` qqDeclsE
                      `extQ` qqIDeclE
                      `extQ` qqIDeclsE
                      `extQ` qqStmE
                      `extQ` qqStmsE
                      `extQ` qqCStmE
                      `extQ` qqCStmsE
                      `extQ` qqSubtypeE
                      `extQ` qqElemAssocListE
                      `extQ` qqRangeE

antiPatQ :: String -> PatQ
antiPatQ = either fail return . parsePat

qqStringP :: String -> Maybe (Q Pat)
qqStringP s = Just $ litP $ stringL s

-- Force a comparison on the String portion of the Symbol. This is because
-- symbols will in general be interned to different identifiers on different
-- runs, so we can't rely on the identifier created when the splice is run.
qqSymbolP :: Symbol -> Maybe (Q Pat)
qqSymbolP (Symbol _ s) = Just [p|Symbol _ $(litP (stringL s))|]

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
                      `extQ` qqSymbolP
                      `extQ` qqLocP
                      `extQ` qqLitP
                      `extQ` qqIdP
                      `extQ` qqExpP
