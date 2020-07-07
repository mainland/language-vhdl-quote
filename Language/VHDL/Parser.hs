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
import Data.Map (Map)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E

import qualified Language.VHDL.Parser.Parser as P
import Language.VHDL.Parser.Monad
import Language.VHDL.Syntax

parse :: [Extension]
      -> Map Name NameSpace
      -> P a
      -> T.Text
      -> Maybe Pos
      -> Either SomeException a
parse exts ns p buf pos =
    evalP p (emptyPState exts ns buf pos)

parseFromFile :: [Extension]
              -> Map Name NameSpace
              -> P a
              -> FilePath
              -> IO a
parseFromFile exts ns p path = do
    text <- liftIO $ B.readFile path
    liftException (parse exts ns p (E.decodeLatin1 text) (Just start))
  where
    start :: Pos
    start = startPos path

parseDesignFile :: [Extension]
                -> Map Name NameSpace
                -> T.Text
                -> Pos
                -> IO [DesignUnit]
parseDesignFile exts ns buf pos = liftException $ parse exts ns P.parseDesignFile buf (Just pos)

parseDesignFileFromFile :: [Extension]
                        -> Map Name NameSpace
                        -> FilePath
                        -> IO [DesignUnit]
parseDesignFileFromFile exts ns = parseFromFile exts ns P.parseDesignFile
