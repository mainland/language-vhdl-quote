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

    PState,
    emptyPState,
    getInput,
    setInput,
    pushLexState,
    popLexState,
    getLexState,
    getPrevToken,
    getCurToken,
    setCurToken,

    useExts,
    antiquotationExt,

    addTypeName,
    isTypeName,

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

    quote
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
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif /* !MIN_VERSION_base(4,20,0) */
import Data.Loc
#if !(MIN_VERSION_base(4,9,0))
import Data.Monoid (Monoid(..), (<>))
#endif /* !(MIN_VERSION_base(4,9,0)) */
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import Data.Word (Word32)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Language.VHDL.Parser.Alex
import Language.VHDL.Parser.Tokens
import Language.VHDL.Parser.Exceptions
import Language.VHDL.Syntax (Extension(..),
                             Id)

type Extensions = Word32

data PState = PState
    { input      :: !AlexInput
    , prevToken  :: Maybe (L Token)
    , curToken   :: Maybe (L Token)
    , lexState   :: ![Int]
    , extensions :: !Extensions
    , typeNames  :: Set Id
    }

emptyPState :: [Extension]
            -> Set Id
            -> T.Text
            -> Maybe Pos
            -> PState
emptyPState exts typens buf pos = PState
    { input      = alexInput buf pos
    , prevToken  = Nothing
    , curToken   = Nothing
    , lexState   = [0]
    , extensions = foldl' setBit 0 (map fromEnum exts)
    , typeNames  = typens
    }

newtype P a = P { runP :: PState -> Either SomeException (a, PState) }

instance Functor P where
    fmap f mx = P $ \s -> case runP mx s of
                            Left e         -> Left e
                            Right (x, s')  -> Right (f x, s')

instance Applicative P where
    pure x = P $ \s -> Right (x, s)

    mf <*> mx = P $ \s -> case runP mf s of
                            Left e         -> Left e
                            Right (f, s')  -> runP (fmap f mx) s'

instance Monad P where
    m >>= k = P $ \s -> case runP m s of
                          Left e         -> Left e
                          Right (a, s')  -> runP (k a) s'

    return = pure

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

getPrevToken :: P (Maybe (L Token))
getPrevToken = gets prevToken

getCurToken :: P (Maybe (L Token))
getCurToken = gets curToken

setCurToken :: L Token -> P ()
setCurToken tok = modify $ \s -> s { prevToken = curToken s
                                   , curToken  = Just tok
                                   }

useExts :: Extensions -> P Bool
useExts ext = gets $ \s -> extensions s .&. ext /= 0

antiquotationExt :: Extensions
antiquotationExt = (bit . fromEnum) Antiquotation

alexGetCharOrFail :: AlexInput -> P (Char, AlexInput)
alexGetCharOrFail inp =
    case alexGetChar inp of
      Nothing         -> unexpectedEOF inp
      Just (c, inp')  -> return (c, inp')

addTypeName :: Id -> P ()
addTypeName n = modify $ \s ->
  s { typeNames = Set.insert n (typeNames s) }

isTypeName :: Id -> P Bool
isTypeName ident =  gets $ \s -> Set.member ident (typeNames s)

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

expected :: [String] -> Maybe Doc -> P b
expected alts after = do
    tok <- getCurToken
    expectedAt tok alts after

expectedAt :: Maybe (L Token) -> [String] -> Maybe Doc -> P b
expectedAt tok alts after = do
    at <- getAt tok
    parserError at $
      text "Expected" <+> pprAlts alts <+> pprGot tok <+> pprAfter after
  where
    getAt :: Maybe (L Token) -> P Loc
    getAt Nothing = do
        maybe_pos <- alexPos <$> getInput
        case maybe_pos of
            Nothing  -> pure NoLoc
            Just pos -> pure $ Loc pos pos

    getAt (Just (L loc _)) =
        pure loc

    pprAlts :: [String] -> Doc
    pprAlts []        = empty
    pprAlts [s]       = text s
    pprAlts [s1, s2]  = text s1 <+> text "or" <+> text s2
    pprAlts (s : ss)  = text s <> comma <+> pprAlts ss

    pprGot :: Maybe (L Token) -> Doc
    pprGot Nothing            = empty
    pprGot (Just (L _ Teof))  = text "but reached end of file"
    pprGot (Just (L _ t))     = text "but got" <+> quote (ppr t)

    pprAfter :: Maybe Doc -> Doc
    pprAfter Nothing     = empty
    pprAfter (Just what) = text "after" <+> what

quote :: Doc -> Doc
quote = enclose (char '`') (char '\'')
