--------------------------------------------------------------------------------
-- |
-- Module      : Language.VHDL.Parser
-- Copyright   : (c) 2015-2016 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>
--
--------------------------------------------------------------------------------

module Language.VHDL.Parser (
    P,
    emptyPState,
    evalP,

    parse,
    parseDesignFile,
    parseDesignFileFromFile
  ) where

import Control.Monad.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import Data.Loc
import Data.Set (Set)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E

import qualified Language.VHDL.Parser.Parser as P
import Language.VHDL.Parser.Monad
import Language.VHDL.Syntax

parse :: [Extension]
      -> Set Id
      -> P a
      -> T.Text
      -> Maybe Pos
      -> Either SomeException a
parse exts types p buf pos =
    evalP p (emptyPState exts types buf pos)

parseFromFile :: [Extension]
              -> Set Id
              -> P a
              -> FilePath
              -> IO a
parseFromFile exts types p path = do
    text <- liftIO $ B.readFile path
    liftException (parse exts types p (E.decodeLatin1 text) (Just start))
  where
    start :: Pos
    start = startPos path

parseDesignFile :: [Extension]
                -> Set Id
                -> T.Text
                -> Pos
                -> IO [DesignUnit]
parseDesignFile exts types buf pos =
    liftException $ parse exts types P.parseDesignFile buf (Just pos)

parseDesignFileFromFile :: [Extension]
                        -> Set Id
                        -> FilePath
                        -> IO [DesignUnit]
parseDesignFileFromFile exts types =
    parseFromFile exts types P.parseDesignFile
