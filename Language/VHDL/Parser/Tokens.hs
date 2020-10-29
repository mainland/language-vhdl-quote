{-# LANGUAGE CPP #-}

-- |
-- Module      : Language.VHDL.Parser.Tokens
-- Copyright   : (c) 2016-2020 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>

module Language.VHDL.Parser.Tokens (
    Token(..)
  ) where

#if !(MIN_VERSION_base(4,9,0))
import Data.Monoid (Monoid(..), (<>))
#endif /* !(MIN_VERSION_base(4,9,0)) */
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Symbol
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

data Token = Teof

           | TintLit (String, Integer)
           | TrealLit (String, Rational)
           | TcharLit (String, Char)
           | TstringLit (String, String)
           | TbitstringLit String
           | Toperator (String, String)

           -- | Identifiers
           | Tident Symbol
           | Text_ident Symbol
           | Ttype_ident Symbol
           | Tfun_ident Symbol
           | Tarr_ident Symbol

           | Tabs
           | Taccess
           | Tafter
           | Talias
           | Tall
           | Tand
           | Tarchitecture
           | Tarray
           | Tassert
           | Tassume
           | Tassume_guarantee
           | Tattribute
           | Tbegin
           | Tblock
           | Tbody
           | Tbuffer
           | Tbus
           | Tcase
           | Tcomponent
           | Tconfiguration
           | Tconstant
           | Tcontext
           | Tcover
           | Tdefault
           | Tdisconnect
           | Tdownto
           | Telse
           | Telsif
           | Tend
           | Tentity
           | Texit
           | Tfairness
           | Tfile
           | Tfor
           | Tforce
           | Tfunction
           | Tgenerate
           | Tgeneric
           | Tgroup
           | Tguarded
           | Tif
           | Timpure
           | Tin
           | Tinertial
           | Tinout
           | Tis
           | Tlabel
           | Tlibrary
           | Tlinkage
           | Tliteral
           | Tloop
           | Tmap
           | Tmod
           | Tnand
           | Tnew
           | Tnext
           | Tnor
           | Tnot
           | Tnull
           | Tof
           | Ton
           | Topen
           | Tor
           | Tothers
           | Tout
           | Tpackage
           | Tparameter
           | Tport
           | Tpostponed
           | Tprocedure
           | Tprocess
           | Tproperty
           | Tprotected
           | Tpure
           | Trange
           | Trecord
           | Tregister
           | Treject
           | Trelease
           | Trem
           | Treport
           | Trestrict
           | Trestrict_guarantee
           | Treturn
           | Trol
           | Tror
           | Tselect
           | Tsequence
           | Tseverity
           | Tshared
           | Tsignal
           | Tsla
           | Tsll
           | Tsra
           | Tsrl
           | Tstrong
           | Tsubtype
           | Tthen
           | Tto
           | Ttransport
           | Ttype
           | Tunaffected
           | Tunits
           | Tuntil
           | Tuse
           | Tvariable
           | Tvmode
           | Tvprop
           | Tvunit
           | Twait
           | Twhen
           | Twhile
           | Twith
           | Txnor
           | Txor

           -- Keywords unique to quasiquoter
           | Ttypename
           | Tfunname
           | Tarrname

           | Tplus   -- ^ @+@
           | Tminus  -- ^ @-@
           | Tcat    -- ^ @&@
           | Tstar   -- ^ @*@
           | Tdiv    -- ^ @/@

           | Tsquote -- ^ @'@
           | Tdot    -- ^ @.@
           | Tcomma  -- ^ @,@
           | Tsemi   -- ^ @;@
           | Tcolon  -- ^ @:@
           | Tbar    -- ^ @|@
           | Tat     -- ^ @\@@

           | Tlparen -- ^ @(@
           | Trparen -- ^ @)@
           | Tlbrack -- ^ @[@
           | Trbrack -- ^ @]@

           | Tarrow  -- ^ @=>@
           | Texp    -- ^ @**@
           | Tassign -- ^ @:=@
           | Tbox    -- ^ @<>@
           | Tcond   -- ^ @??@
           | Tdlt    -- ^ @<<@
           | Tdgt    -- ^ @>>@

           | Teq -- ^ @=@
           | Tne -- ^ @/=@
           | Tlt -- ^ @<@
           | Tle -- ^ @<=@
           | Tge -- ^ @>=@
           | Tgt -- ^ @>@

           | Tmatch    -- ^ @?@
           | Tmatch_eq -- ^ @?=@
           | Tmatch_ne -- ^ @?/=@
           | Tmatch_lt -- ^ @?<@
           | Tmatch_le -- ^ @?<=@
           | Tmatch_ge -- ^ @?>=@
           | Tmatch_gt -- ^ @?>@

           | Tanti_id String
           | Tanti_int String
           | Tanti_real String
           | Tanti_char String
           | Tanti_string String
           | Tanti_bitstring String
           | Tanti_lit String
           | Tanti_lits String

           | Tanti_type String

           | Tanti_exp String
           | Tanti_exps String

           | Tanti_decl String
           | Tanti_decls String

           | Tanti_idecl String
           | Tanti_idecls String

           | Tanti_stm String
           | Tanti_stms String

           | Tanti_cstm String
           | Tanti_cstms String
  deriving (Eq, Ord, Read, Show)


pprSym :: Symbol -> Doc
pprSym = quote . text . unintern
  where
    quote = enclose (char '`') (char '\'')

instance Pretty Token where
    ppr Teof = text "end of file"

    ppr (TintLit (s, _))    = text s
    ppr (TrealLit (s, _))   = text s
    ppr (TcharLit (s, _))   = text s
    ppr (TstringLit (s, _)) = text s
    ppr (TbitstringLit s)   = text s
    ppr (Toperator (s, _))  = text s

    ppr (Tident sym)      = text "identifier" <+> pprSym sym
    ppr (Text_ident sym)  = text "external identifier" <+> pprSym sym
    ppr (Ttype_ident sym) = text "type identifier" <+> pprSym sym
    ppr (Tfun_ident sym)  = text "function identifier" <+> pprSym sym
    ppr (Tarr_ident sym)  = text "array identifier" <+> pprSym sym

    ppr Tabs                = text "abs"
    ppr Taccess             = text "access"
    ppr Tafter              = text "after"
    ppr Talias              = text "alias"
    ppr Tall                = text "all"
    ppr Tand                = text "and"
    ppr Tarchitecture       = text "architecture"
    ppr Tarray              = text "array"
    ppr Tassert             = text "assert"
    ppr Tassume             = text "assume"
    ppr Tassume_guarantee   = text "assums_guarantee"
    ppr Tattribute          = text "attribute"
    ppr Tbegin              = text "begin"
    ppr Tblock              = text "block"
    ppr Tbody               = text "body"
    ppr Tbuffer             = text "buffer"
    ppr Tbus                = text "bus"
    ppr Tcase               = text "case"
    ppr Tcomponent          = text "component"
    ppr Tconfiguration      = text "configuration"
    ppr Tconstant           = text "constant"
    ppr Tcontext            = text "context"
    ppr Tcover              = text "cover"
    ppr Tdefault            = text "default"
    ppr Tdisconnect         = text "disconnect"
    ppr Tdownto             = text "downto"
    ppr Telse               = text "else"
    ppr Telsif              = text "elsif"
    ppr Tend                = text "end"
    ppr Tentity             = text "entity"
    ppr Texit               = text "exit"
    ppr Tfairness           = text "fairness"
    ppr Tfile               = text "file"
    ppr Tfor                = text "for"
    ppr Tforce              = text "force"
    ppr Tfunction           = text "function"
    ppr Tgenerate           = text "generate"
    ppr Tgeneric            = text "generic"
    ppr Tgroup              = text "group"
    ppr Tguarded            = text "guarded"
    ppr Tif                 = text "if"
    ppr Timpure             = text "impure"
    ppr Tin                 = text "in"
    ppr Tinertial           = text "inertial"
    ppr Tinout              = text "inout"
    ppr Tis                 = text "is"
    ppr Tlabel              = text "label"
    ppr Tlibrary            = text "library"
    ppr Tlinkage            = text "linkage"
    ppr Tliteral            = text "literal"
    ppr Tloop               = text "loop"
    ppr Tmap                = text "map"
    ppr Tmod                = text "mod"
    ppr Tnand               = text "nand"
    ppr Tnew                = text "new"
    ppr Tnext               = text "next"
    ppr Tnor                = text "nor"
    ppr Tnot                = text "not"
    ppr Tnull               = text "null"
    ppr Tof                 = text "of"
    ppr Ton                 = text "on"
    ppr Topen               = text "open"
    ppr Tor                 = text "or"
    ppr Tothers             = text "others"
    ppr Tout                = text "out"
    ppr Tpackage            = text "package"
    ppr Tparameter          = text "parameter"
    ppr Tport               = text "port"
    ppr Tpostponed          = text "potponed"
    ppr Tprocedure          = text "procedure"
    ppr Tprocess            = text "process"
    ppr Tproperty           = text "property"
    ppr Tprotected          = text "protected"
    ppr Tpure               = text "pure"
    ppr Trange              = text "range"
    ppr Trecord             = text "record"
    ppr Tregister           = text "register"
    ppr Treject             = text "reject"
    ppr Trelease            = text "release"
    ppr Trem                = text "rem"
    ppr Treport             = text "report"
    ppr Trestrict           = text "restrict"
    ppr Trestrict_guarantee = text "Trestrict_guarantee"
    ppr Treturn             = text "return"
    ppr Trol                = text "rol"
    ppr Tror                = text "ror"
    ppr Tselect             = text "select"
    ppr Tsequence           = text "sequence"
    ppr Tseverity           = text "severity"
    ppr Tshared             = text "shared"
    ppr Tsignal             = text "signal"
    ppr Tsla                = text "sla"
    ppr Tsll                = text "sll"
    ppr Tsra                = text "sra"
    ppr Tsrl                = text "srl"
    ppr Tstrong             = text "strong"
    ppr Tsubtype            = text "subtype"
    ppr Tthen               = text "then"
    ppr Tto                 = text "to"
    ppr Ttransport          = text "transport"
    ppr Ttype               = text "type"
    ppr Tunaffected         = text "unaffected"
    ppr Tunits              = text "units"
    ppr Tuntil              = text "until"
    ppr Tuse                = text "use"
    ppr Tvariable           = text "variable"
    ppr Tvmode              = text "vmode"
    ppr Tvprop              = text "vprop"
    ppr Tvunit              = text "vunit"
    ppr Twait               = text "wait"
    ppr Twhen               = text "when"
    ppr Twhile              = text "while"
    ppr Twith               = text "with"
    ppr Txnor               = text "xnor"
    ppr Txor                = text "xor"

    ppr Ttypename = text "typename"
    ppr Tfunname  = text "funname"
    ppr Tarrname  = text "arrname"

    ppr Tplus   = text "+"
    ppr Tminus  = text "-"
    ppr Tcat    = text "&"
    ppr Tstar   = text "*"
    ppr Tdiv    = text "/"

    ppr Tsquote = text "'"
    ppr Tdot    = text "."
    ppr Tcomma  = text ","
    ppr Tsemi   = text ";"
    ppr Tcolon  = text ":"
    ppr Tbar    = text "|"
    ppr Tat     = text "@"

    ppr Tlparen = text "("
    ppr Trparen = text ")"
    ppr Tlbrack = text "["
    ppr Trbrack = text "]"

    ppr Tarrow  = text "=>"
    ppr Texp    = text "**"
    ppr Tassign = text ":="
    ppr Tbox    = text "<>"
    ppr Tcond   = text "??"
    ppr Tdlt    = text "<<"
    ppr Tdgt    = text ">>"

    ppr Teq = text "="
    ppr Tne = text "/="
    ppr Tlt = text "<"
    ppr Tle = text "<="
    ppr Tge = text ">="
    ppr Tgt = text ">"

    ppr Tmatch    = text "?"
    ppr Tmatch_eq = text "?="
    ppr Tmatch_ne = text "?/="
    ppr Tmatch_lt = text "?<"
    ppr Tmatch_le = text "?<="
    ppr Tmatch_ge = text "?>="
    ppr Tmatch_gt = text "?>"

    ppr (Tanti_id e)        = pprAnti "id" e
    ppr (Tanti_int e)       = pprAnti "int" e
    ppr (Tanti_real e)      = pprAnti "real" e
    ppr (Tanti_char e)      = pprAnti "char" e
    ppr (Tanti_string e)    = pprAnti "string" e
    ppr (Tanti_bitstring e) = pprAnti "bitstring" e

    ppr (Tanti_lit e)       = pprAnti "lit" e
    ppr (Tanti_lits e)      = pprAnti "lits" e

    ppr (Tanti_type e) = pprAnti "ty" e

    ppr (Tanti_exp e)  = pprAnti "exp" e
    ppr (Tanti_exps e) = pprAnti "exps" e

    ppr (Tanti_decl e)  = pprAnti "decl" e
    ppr (Tanti_decls e) = pprAnti "decls" e

    ppr (Tanti_idecl e)  = pprAnti "idecl" e
    ppr (Tanti_idecls e) = pprAnti "idecls" e

    ppr (Tanti_stm e)  = pprAnti "stm" e
    ppr (Tanti_stms e) = pprAnti "stms" e

    ppr (Tanti_cstm e)  = pprAnti "cstm" e
    ppr (Tanti_cstms e) = pprAnti "cstms" e

pprAnti :: Pretty a => String -> a -> Doc
pprAnti anti x = char '$' <> text anti <> colon <> ppr x
