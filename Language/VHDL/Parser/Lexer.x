{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -w #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Language.VHDL.Parser.Lexer
-- Copyright   : (c) 2014-2016 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>
--
--------------------------------------------------------------------------------

module Language.VHDL.Parser.Lexer (
    lexToken,
    lexTokens
  ) where

import Prelude hiding (exponent)

import Control.Applicative
import Control.Monad (unless,
                      void,
                      when)
import Control.Monad.Exception
import Control.Monad.Reader (MonadReader,
                             ask)
import Control.Monad.State (get)
import Data.Char (chr,
                  isAlphaNum,
                  isDigit,
                  isHexDigit,
                  isLower,
                  isOctDigit,
                  ord,
                  toLower)
import Data.List (intersperse)
import Data.Loc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Symbol
import qualified Data.Text.Lazy as T
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadP hiding (get)
import Text.PrettyPrint.Mainland (text)

import Language.VHDL.Parser.Alex
import Language.VHDL.Parser.Monad
import Language.VHDL.Parser.Tokens
import Language.VHDL.Syntax (NoCase(..), Id(..), mkNoCase)
}

$upper         = [A-ZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ]
$lower         = [a-zßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ]
$special       = [\]\"\#\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\]\_\`\|] -- "
$other_special = [\!\$\%\^\{\}~¡¢£¤¥¦§©«¬®°±23μ¶·1o»¿×÷\-]
$space         = [\ \t\n\r\f\v]

$digit = [0-9]
$octit = [0-7]
$hexit = [0-9 A-F a-f]

$letter          = [$upper $lower]
$letter_or_digit = [$letter $digit]
$extended_digit  = [$digit $letter]

$basic_graphic = [$upper $digit $special $space]
$graphic       = [$basic_graphic $lower $other_special]
$basic_char    = [$basic_graphic]

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+

@integer       = $digit ("_"? $digit)*
@based_integer = $extended_digit ("_"? $extended_digit)*

@exponent = [eE] [\-\+]? @integer

@char   = \' $graphic \'
@string = \" ($graphic # [\"] | \"\")* \" -- "

@base      = "B" | "O" | "X" | "UB" | "UO" | "UX" | "SB" | "SO" | "SX" | "D"
@bitstring = @integer? @base \"  $graphic ("_" $graphic)* \"

@singcomment  = "--" .*
@multicomment = "/*" ([^\*]|[\r\n]|("*"+([^\*\/]|[\r\n])))* "*"+ "/"

vhdl :-
<0> {
  "$"      / { allowAnti } { lexAnti Tanti_exp }
  "$id:"   / { allowAnti } { lexAnti Tanti_id }
  "$exp:"  / { allowAnti } { lexAnti Tanti_exp }
  "$exps:" / { allowAnti } { lexAnti Tanti_exps }
  "$int:"  / { allowAnti } { lexAnti Tanti_int }
  "$real:" / { allowAnti } { lexAnti Tanti_real }
  "$lit:"  / { allowAnti } { lexAnti Tanti_lit }
  "$lits:" / { allowAnti } { lexAnti Tanti_lits }
  "$ty:"   / { allowAnti } { lexAnti Tanti_type }
}

<0> {
  ^ $space* "#line" $space+ $digit+ $space+ \" [^\"]* \" .* { setLineFromPragma } -- "
  ^ $space* "#"     $space+ $digit+ $space+ \" [^\"]* \" .* { setLineFromPragma } -- "

  @singcomment ;
  @multicomment ;

  $space+ ;

  $letter ("_"? $letter_or_digit)* { identifier }
  "\\" ($graphic | "\\\\")+ "\\"   { extidentifier }

  @integer @exponent?              { lexLit intLit TintLit }
  @integer "." @integer @exponent? { lexLit realLit TrealLit }

  @integer "#" @based_integer "#"  @exponent?                   { lexLit basedIntLit TintLit }
  @integer "#" @based_integer "." @based_integer "#" @exponent? { lexLit basedRealLit TrealLit }

  @char      { lexCharLit }
  @string    { lexStringLit }
  @bitstring { lexBitStringLit }

  "+"   { token Tplus }
  "-"   { token Tminus }
  "&"   { token Tcat }
  "*"   { token Tstar }
  "/"   { token Tdiv }

  \'    { token Tsquote }
  "."   { token Tdot }
  ","   { token Tcomma }
  ";"   { token Tsemi }
  ":"   { token Tcolon }
  "|"   { token Tbar }
  "@"   { token Tat }

  "("   { token Tlparen }
  ")"   { token Trparen }
  "["   { token Tlbrack }
  "]"   { token Trbrack }

  "=>"  { token Tarrow }
  "**"  { token Texp }
  ":="  { token Tassign }
  "<>"  { token Tbox }
  "??"  { token Tcond }
  "<<"  { token Tdlt }
  ">>"  { token Tdgt }

  "="    { token Teq }
  "/="   { token Tne }
  "<"    { token Tlt }
  "<="   { token Tle }
  ">="   { token Tge }
  ">"    { token Tgt }

  "?"    { token Tmatch }
  "?="   { token Tmatch_eq }
  "?/="  { token Tmatch_ne }
  "?<"   { token Tmatch_lt }
  "?<="  { token Tmatch_le }
  "?>="  { token Tmatch_ge }
  "?>"   { token Tmatch_gt }
}

{
keywords :: Map Symbol Token
keywords = Map.fromList kws
  where
    kws :: [(Symbol, Token)]
    kws = [ ("abs", Tabs)
          , ("access", Taccess)
          , ("after", Tafter)
          , ("alias", Talias)
          , ("all", Tall)
          , ("and", Tand)
          , ("architecture", Tarchitecture)
          , ("array", Tarray)
          , ("arrname", Tarrname)
          , ("assert", Tassert)
          , ("assume", Tassume)
          , ("assume_guarantee", Tassume_guarantee)
          , ("attribute", Tattribute)
          , ("begin", Tbegin)
          , ("block", Tblock)
          , ("body", Tbody)
          , ("buffer", Tbuffer)
          , ("bus", Tbus)
          , ("case", Tcase)
          , ("component", Tcomponent)
          , ("configuration", Tconfiguration)
          , ("constant", Tconstant)
          , ("context", Tcontext)
          , ("cover", Tcover)
          , ("default", Tdefault)
          , ("disconnect", Tdisconnect)
          , ("downto", Tdownto)
          , ("else", Telse)
          , ("elsif", Telsif)
          , ("end", Tend)
          , ("entity", Tentity)
          , ("exit", Texit)
          , ("fairness", Tfairness)
          , ("file", Tfile)
          , ("for", Tfor)
          , ("force", Tforce)
          , ("function", Tfunction)
          , ("funname", Tfunname)
          , ("generate", Tgenerate)
          , ("generic", Tgeneric)
          , ("group", Tgroup)
          , ("guarded", Tguarded)
          , ("if", Tif)
          , ("impure", Timpure)
          , ("in", Tin)
          , ("inertial", Tinertial)
          , ("inout", Tinout)
          , ("is", Tis)
          , ("label", Tlabel)
          , ("library", Tlibrary)
          , ("linkage", Tlinkage)
          , ("literal", Tliteral)
          , ("loop", Tloop)
          , ("map", Tmap)
          , ("mod", Tmod)
          , ("nand", Tnand)
          , ("new", Tnew)
          , ("next", Tnext)
          , ("nor", Tnor)
          , ("not", Tnot)
          , ("null", Tnull)
          , ("of", Tof)
          , ("on", Ton)
          , ("open", Topen)
          , ("or", Tor)
          , ("others", Tothers)
          , ("out", Tout)
          , ("package", Tpackage)
          , ("parameter", Tparameter)
          , ("port", Tport)
          , ("postponed", Tpostponed)
          , ("procedure", Tprocedure)
          , ("process", Tprocess)
          , ("property", Tproperty)
          , ("protected", Tprotected)
          , ("pure", Tpure)
          , ("range", Trange)
          , ("record", Trecord)
          , ("register", Tregister)
          , ("reject", Treject)
          , ("release", Trelease)
          , ("rem", Trem)
          , ("report", Treport)
          , ("restrict", Trestrict)
          , ("restrict_guarantee", Trestrict_guarantee)
          , ("return", Treturn)
          , ("rol", Trol)
          , ("ror", Tror)
          , ("select", Tselect)
          , ("sequence", Tsequence)
          , ("severity", Tseverity)
          , ("shared", Tshared)
          , ("signal", Tsignal)
          , ("sla", Tsla)
          , ("sll", Tsll)
          , ("sra", Tsra)
          , ("srl", Tsrl)
          , ("strong", Tstrong)
          , ("subtype", Tsubtype)
          , ("then", Tthen)
          , ("to", Tto)
          , ("transport", Ttransport)
          , ("type", Ttype)
          , ("typename", Ttypename)
          , ("unaffected", Tunaffected)
          , ("units", Tunits)
          , ("until", Tuntil)
          , ("use", Tuse)
          , ("variable", Tvariable)
          , ("vmode", Tvmode)
          , ("vprop", Tvprop)
          , ("vunit", Tvunit)
          , ("wait", Twait)
          , ("when", Twhen)
          , ("while", Twhile)
          , ("with", Twith)
          , ("xnor", Txnor)
          , ("xor", Txor)
          ]

isOperator :: String -> Bool
isOperator = (`elem` operators)
  where
    operators :: Set String
    operators = Set.fromList ["??"
                            , "and", "or", "nand", "nor", "xor", "xnor"
                            , "=", "/=", "<", "<=", ">", ">="
                            , "?=", "?/=", "?<", "?<=", "?>", "?>="
                            , "sll", "srl", "sla", "sra", "rol", "ror"
                            , "+", "–", "&"
                            , "*", "/", "mod", "rem"
                            , "**", "abs", "not"]

identifier :: Action P Token
identifier beg end =
    case Map.lookup ident keywords of
      Nothing  -> do ns <- lookupNamespace (Id (mkNoCase ident) noLoc)
                     case ns of
                       TypeN  -> token (Ttype_ident ident) beg end
                       FunN   -> token (Tfun_ident ident) beg end
                       ArrN   -> token (Tarr_ident ident) beg end
                       OtherN -> token (Tident ident) beg end
      Just tok -> token tok beg end
  where
    ident :: Symbol
    ident = intern (inputString beg end)

extidentifier :: Action P Token
extidentifier beg end =
    token (Text_ident ident) beg end
  where
    ident :: Symbol
    ident = intern (inputString beg end)

lexLit :: Show a => ReadP a -> ((String, a) -> Token) -> Action P Token
lexLit p tok beg end = do
    case readP_to_S (p <* eof) s of
      [(x, "")] -> return $ locateTok beg end $ tok (s, x)
      xs         -> fail $ "Cannot parse literal: " ++ show xs
  where
    s :: String
    s = inputString beg end

intLit :: ReadP Integer
intLit = do
    i <- basedInteger 10
    e <- optionalExponent
    return $ i*10^e

realLit :: ReadP Rational
realLit = do
    r <- basedRational 10
    e <- optionalExponent
    return $ r*10^^fromIntegral e

basedIntLit :: ReadP Integer
basedIntLit = do
    (base, x, e) <- basedP basedInteger
    return (x*base^e)

basedRealLit :: ReadP Rational
basedRealLit = do
    (base, x, e) <- basedP basedRational
    return (x*fromIntegral base^^e)

basedP :: (Show a, Num a) => (Integer -> ReadP a) -> ReadP (Integer, a, Integer)
basedP p = do
    base <- basedInteger 10
    x    <- between hash hash (p base)
    e    <- optionalExponent
    return (base, x, e)
  where
    hash :: ReadP Char
    hash = char '#'

basedInteger :: Integer -> ReadP Integer
basedInteger base = do
    ds <- digits base
    return $ digitsToInteger base ds

basedRational :: Integer -> ReadP Rational
basedRational base = do
    n <- basedInteger base
    void $ char '.'
    ds    <- digits base
    let m =  digitsToInteger base ds
    return $ (n*base^length ds + m) % (base^length ds)

optionalExponent :: ReadP Integer
optionalExponent = option 0 exponent

exponent :: ReadP Integer
exponent = do
    void $ satisfy (\c -> toLower c == 'e')
    f <- sign <|> return id
    i <- basedInteger 10
    return $ f i

sign :: Num a => ReadP (a -> a)
sign = do
    c <- ReadP.get
    case c of
      '+' -> return id
      '-' -> return negate
      _   -> fail "Not a sign"

digit :: Integer -> ReadP Integer
digit base = do
    c <- satisfy isBaseDigit
    i <- toBaseDigit c
    unless (i < base) $
        fail "Bad digit"
    return i
  where
    isBaseDigit :: Char -> Bool
    isBaseDigit c
        | '0' <= c && c <= '9' = toDigit '0' c < base
        | 'a' <= c && c <= 'f' = 10 + toDigit 'a' c < base
        | 'A' <= c && c <= 'F' = 10 + toDigit 'A' c < base
        | otherwise            = False

    toBaseDigit :: Monad m => Char -> m Integer
    toBaseDigit c
        | '0' <= c && c <= '9' = return $ toDigit '0' c
        | 'a' <= c && c <= 'f' = return $ 10 + toDigit 'a' c
        | 'A' <= c && c <= 'F' = return $ 10 + toDigit 'A' c
        | otherwise            = error "Bad digit"

    toDigit :: Char -> Char -> Integer
    toDigit b c = fromIntegral $ ord c - ord b

digits :: Integer -> ReadP [Integer]
digits base = catMaybes <$> many1 (justDigit <|> underscore)
  where
    justDigit :: ReadP (Maybe Integer)
    justDigit = Just <$> digit base

    underscore :: ReadP (Maybe a)
    underscore = char '_' >> return Nothing

digitsToInteger :: Integer -> [Integer] -> Integer
digitsToInteger base ds = go 0 ds
  where
    go :: Integer -> [Integer] -> Integer
    go !i []     = i
    go !i (d:ds) = go (i*base+d) ds

lexCharLit :: Action P Token
lexCharLit beg end = do
    let raw :: String
        raw = inputString beg end
        c :: Char
        c = lexChar raw
    return $ locateTok beg end (TcharLit (raw, c))
  where
    lexChar :: String -> Char
    lexChar ('\'' : c : "'") = c

lexStringLit :: Action P Token
lexStringLit beg end = do
    let raw :: String
        raw = inputString beg end
        s :: String
        s = lexString (tail raw)
    if isOperator s
      then return $ locateTok beg end (Toperator (raw, s))
      else return $ locateTok beg end (TstringLit (raw, s))
  where
    lexString :: String -> String
    lexString []               = error "can't happen"
    lexString ['"']            = ""
    lexString ('"' : '"' : cs) = '"' : lexString cs
    lexString (x : cs)         = x : lexString cs

lexBitStringLit :: Action P Token
lexBitStringLit beg end =
    return $ locateTok beg end (TbitstringLit (inputString beg end))

setLineFromPragma :: Action P Token
setLineFromPragma beg end = do
    inp <- getInput
    setInput inp { alexPos = pos' }
    lexToken
  where
    (_ : l : ws) = words (inputString beg end)
    line = read l - 1
    filename = (takeWhile (/= '\"') . drop 1 . concat . intersperse " ") ws

    pos' :: Maybe Pos
    pos' = case alexPos beg of
             Nothing  -> Nothing
             Just pos -> Just $ Pos filename line 1 (posCoff pos)

lexAnti :: (String -> Token) -> Action P Token
lexAnti antiTok beg end = do
    c <- nextChar
    s <- case c of
           '('                 -> lexExpression 0 ""
           _ | isIdStartChar c -> lexIdChars [c]
             | otherwise       -> lexerError beg (text "illegal antiquotation")
    return $ locateTok beg end (antiTok s)
  where
    lexIdChars :: String -> P String
    lexIdChars s = do
        maybe_c <- maybePeekChar
        case maybe_c of
          Just c | isIdChar c -> skipChar >> lexIdChars (c : s)
          _                   -> return (reverse s)

    lexExpression :: Int -> String -> P String
    lexExpression depth s = do
        maybe_c <- maybePeekChar
        case maybe_c of
          Nothing               -> do end <- getInput
                                      parserError (alexLoc beg end)
                                                  (text "unterminated antiquotation")
          Just '('              -> skipChar >> lexExpression (depth+1) ('(' : s)
          Just ')' | depth == 0 -> skipChar >> return (unescape (reverse s))
                   | otherwise  -> skipChar >> lexExpression (depth-1) (')' : s)
          Just c                -> skipChar >> lexExpression depth (c : s)
      where
        unescape :: String -> String
        unescape ('\\':'|':'\\':']':s)  = '|' : ']' : unescape s
        unescape (c:s)                  = c : unescape s
        unescape []                     = []

    isIdStartChar :: Char -> Bool
    isIdStartChar '_' = True
    isIdStartChar c   = isLower c

    isIdChar :: Char -> Bool
    isIdChar '_'  = True
    isIdChar '\'' = True
    isIdChar c    = isAlphaNum c

lexToken :: P (L Token)
lexToken = do
    beg  <- getInput
    st   <- get
    case alexScanUser st beg 0 of
      AlexEOF ->
          return $ L (alexLoc beg beg) Teof
      AlexError end ->
          lexerError end (text rest)
        where
          rest :: String
          rest = T.unpack $ T.take 80 $ T.drop (alexOff end) (alexBuf end)

      AlexSkip end _ ->
          setInput end >> lexToken

      AlexToken end _ t ->
          setInput end >> t beg end

lexTokens :: MonadException m
          => T.Text
          -> Pos
          -> m [L Token]
lexTokens buf start =
    liftException (evalP tokens (emptyPState [] mempty buf (Just start)))
  where
    tokens :: P [L Token]
    tokens = do
        t <- lexToken
        case t of
          L _ Teof  -> return [t]
          _         -> (t :) <$> tokens
}
