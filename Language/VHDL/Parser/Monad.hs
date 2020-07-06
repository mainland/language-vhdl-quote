{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Language.VHDL.Parser.Monad
-- Copyright   : (c) 2014-2020 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>

module Language.VHDL.Parser.Monad (
    P,
    runP,
    evalP,

    NameSpace(..),

    PState,
    emptyPState,
    getInput,
    setInput,
    pushLexState,
    popLexState,
    getLexState,
    getCurToken,
    setCurToken,

    useExts,
    antiquotationExt,

    putPrefix,
    getPrefix,

    addNamespace,
    addTypeName,
    addFunName,
    addFunBaseName,
    addArrName,
    lookupNamespace,

    alexGetCharOrFail,
    maybePeekChar,
    peekChar,
    peekChars,
    nextChar,
    skipChar,

    AlexPredicate,
    allowAnti,
    ifExtension,

    failAt,
    lexerError,
    illegalIntLiteral,
    illegalRealLiteral,
    illegalCharacterLiteral,
    unexpectedEOF,

    parserError,
    unclosed,
    expected,
    expectedAt,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative(..))
#endif /* !MIN_VERSION_base(4,8,0) */
import Control.Monad.Exception
import Control.Monad.State
import Data.Bits ((.&.),
                  bit,
                  setBit)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Loc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
#if !(MIN_VERSION_base(4,9,0))
import Data.Monoid (Monoid(..), (<>))
#endif /* !(MIN_VERSION_base(4,9,0)) */
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import qualified Data.Text.Lazy as T
import Data.Word (Word32)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Language.VHDL.Parser.Alex
import Language.VHDL.Parser.Tokens
import Language.VHDL.Parser.Exceptions
import Language.VHDL.Syntax (BaseName(..),
                             Extension(..),
                             Id,
                             Name(..),
                             NameSpace(..),
                             mkIdName)

type Extensions = Word32

data PState = PState
    { input      :: !AlexInput
    , curToken   :: L Token
    , lexState   :: ![Int]
    , extensions :: !Extensions
    , nsPrefix   :: [Id]
    , nsIdents   :: Map Name NameSpace
    }

emptyPState :: [Extension]
            -> Map Name NameSpace
            -> T.Text
            -> Maybe Pos
            -> PState
emptyPState exts ns buf pos = PState
    { input      = alexInput buf pos
    , curToken   = error "no token"
    , lexState   = [0]
    , extensions = foldl' setBit 0 (map fromEnum exts)
    , nsPrefix   = []
    , nsIdents   = ns
    }

newtype P a = P { runP :: PState -> Either SomeException (a, PState) }

instance Functor P where
    fmap f x = x >>= return . f

instance Applicative P where
    pure a = P $ \s -> Right (a, s)

    (<*>) = ap

instance Monad P where
    return = pure

    m >>= k = P $ \s ->
        case runP m s of
          Left e         -> Left e
          Right (a, s')  -> runP (k a) s'

#if MIN_VERSION_base(4,13,0)
instance MonadFail P where
#endif
    fail msg = do
        inp <- getInput
        throw $ ParserException (alexLoc inp inp) (text msg)

instance MonadState PState P where
    get    = P $ \s -> Right (s, s)
    put s  = P $ \_ -> Right ((), s)

instance MonadException P where
    throw e = P $ \_ -> Left (toException e)

    m `catch` h = P $ \s ->
        case runP m s of
          Left e ->
              case fromException e of
                Just e'  -> runP (h e') s
                Nothing  -> Left e
          Right (a, s')  -> Right (a, s')

evalP :: P a -> PState -> Either SomeException a
evalP comp st =
    case runP comp st of
      Left e        -> Left e
      Right (a, _)  -> Right a

getInput  :: P AlexInput
getInput = gets input

setInput  :: AlexInput -> P ()
setInput inp = modify $ \s ->
    s { input = inp }

pushLexState :: Int -> P ()
pushLexState ls = modify $ \s ->
    s { lexState = ls : lexState s }

popLexState :: P Int
popLexState = do
    ls <- getLexState
    modify $ \s ->
        s { lexState = tail (lexState s) }
    return ls

getLexState :: P Int
getLexState = gets (head . lexState)

getCurToken :: P (L Token)
getCurToken = gets curToken

setCurToken :: L Token -> P ()
setCurToken tok = modify $ \s -> s { curToken = tok }

useExts :: Extensions -> P Bool
useExts ext = gets $ \s -> extensions s .&. ext /= 0

antiquotationExt :: Extensions
antiquotationExt = (bit . fromEnum) Antiquotation

alexGetCharOrFail :: AlexInput -> P (Char, AlexInput)
alexGetCharOrFail inp =
    case alexGetChar inp of
      Nothing         -> unexpectedEOF inp
      Just (c, inp')  -> return (c, inp')

putPrefix :: [Id] -> P ()
putPrefix ids = modify $ \s -> s { nsPrefix = ids }

getPrefix :: P [Id]
getPrefix = gets nsPrefix

addNamespace :: Map Name NameSpace -> P ()
addNamespace m = modify $ \s -> s { nsIdents = m `Map.union` nsIdents s }

addName :: Name -> NameSpace -> P ()
addName n ns = modify $ \s ->
  s { nsIdents = Map.insert n ns (nsIdents s) }

addTypeName :: Id -> P ()
addTypeName ident = addName (mkIdName ident) TypeN

addFunName :: Id -> P ()
addFunName ident = addName (mkIdName ident) FunN

addFunBaseName :: BaseName -> P ()
addFunBaseName (IdN ident _) = addFunName ident
addFunBaseName _             = return ()

addArrName :: Id -> P ()
addArrName ident = addName (mkIdName ident) ArrN

lookupNamespace :: Id -> P NameSpace
lookupNamespace ident = do
    ids <- getPrefix
    let n :: Name
        n = Name ids (IdN ident noLoc) noLoc
    maybe_ns <- gets $ \s -> Map.lookup n (nsIdents s)
    return $ fromMaybe OtherN maybe_ns

maybePeekChar :: P (Maybe Char)
{-# INLINE maybePeekChar #-}
maybePeekChar = do
    inp <- getInput
    case T.uncons (alexBuf inp) of
      Nothing      -> return Nothing
      Just (c, _)  -> return (Just c)

peekChar :: P Char
{-# INLINE peekChar #-}
peekChar = do
    inp <- getInput
    case T.uncons (alexBuf inp) of
      Nothing      -> unexpectedEOF inp
      Just (c, _)  -> return c

peekChars :: Int64 -> P String
{-# INLINE peekChars #-}
peekChars n = do
    inp    <-  getInput
    let s  =   T.take n (alexBuf inp)
    if T.length s < n
      then unexpectedEOF inp
      else return (T.unpack s)

nextChar :: P Char
{-# INLINE nextChar #-}
nextChar = do
    inp <- getInput
    case alexGetChar inp of
      Nothing         -> unexpectedEOF inp
      Just (c, inp')  -> setInput inp' >> return c

skipChar :: P ()
{-# INLINE skipChar #-}
skipChar = do
    inp <- getInput
    case alexGetChar inp of
      Nothing         -> unexpectedEOF inp
      Just (_, inp')  -> setInput inp'

-- | The components of an 'AlexPredicate' are the predicate state, input stream
-- before the token, length of the token, input stream after the token.
type AlexPredicate =  PState
                   -> AlexInput
                   -> Int
                   -> AlexInput
                   -> Bool

allowAnti :: AlexPredicate
allowAnti = ifExtension antiquotationExt

ifExtension :: Extensions -> AlexPredicate
ifExtension i s _ _ _ = extensions s .&. i /= 0

lexerError :: AlexInput -> Doc -> P a
lexerError inp s =
    throw $ LexerException (alexPos inp) (text "lexer error on" <+> s)

illegalIntLiteral :: AlexInput -> P a
illegalIntLiteral inp =
    lexerError inp (text "illegal integer literal")

illegalRealLiteral :: AlexInput -> P a
illegalRealLiteral inp =
    lexerError inp (text "illegal real literal")

illegalCharacterLiteral :: AlexInput -> P a
illegalCharacterLiteral inp =
    lexerError inp (text "illegal character literal")

unexpectedEOF :: AlexInput -> P a
unexpectedEOF inp =
    lexerError inp (text "unexpected end of file")

failAt :: Loc -> String -> P a
failAt loc msg =
    throw $ ParserException loc (text msg)

parserError :: Located a => a -> Doc -> P b
parserError loc msg =
    throw $ ParserException (locOf loc) msg

unclosed :: Loc -> String -> P a
unclosed loc x =
    parserError (locEnd loc) (text "unclosed" <+> quote (text x))

expected :: [String] -> Maybe String -> P b
expected alts after = do
    tok <- getCurToken
    expectedAt tok alts after

expectedAt :: L Token -> [String] -> Maybe String -> P b
expectedAt tok@(L loc _) alts after =
    parserError (locStart loc) (text "Expected" <+> pprAlts alts <+> pprGot tok <> pprAfter after)
  where
    pprAlts :: [String] -> Doc
    pprAlts []        = empty
    pprAlts [s]       = text s
    pprAlts [s1, s2]  = text s1 <+> text "or" <+> text s2
    pprAlts (s : ss)  = text s <> comma <+> pprAlts ss

    pprGot :: L Token -> Doc
    pprGot (L _ Teof)  = text "but reached end of file"
    pprGot (L _ t)     = text "but got" <+> quote (ppr t)

    pprAfter :: Maybe String -> Doc
    pprAfter Nothing     = empty
    pprAfter (Just what) = text " after" <+> text what

quote :: Doc -> Doc
quote = enclose (char '`') (char '\'')
