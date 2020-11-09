{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as E
import Data.Loc
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class (ppr)

import qualified Language.VHDL.Parser as P (P, emptyPState, evalP, parse)
import qualified Language.VHDL.Parser.Lexer as L
import qualified Language.VHDL.Parser.Parser as P
import qualified Language.VHDL.Parser.Tokens as T
import qualified Language.VHDL.Syntax as V

import Opts

defaultTypes :: Set V.Id
defaultTypes = Set.fromList
  [ "Word"
  , "Bit"
  , "Natural"
  ]

main :: IO ()
main = do
    args <- getArgs
    (flags, files) <- compilerOpts args
    let exts = [ext | (f, ext) <- extsMap, f `elem` flags]
    let doTokens = Tokens `elem` flags
    case length files of
      0 -> return ()
      _ -> do  when doTokens $ mapM_ (lexFile exts) files
               mapM_ (parseFile flags exts) files
  where
    extsMap :: [(Flag, V.Extension)]
    extsMap = []

lexFile :: [V.Extension] -> String -> IO ()
lexFile exts filename = do
    text <- E.decodeLatin1 <$> B.readFile filename
    case tokens text of
      Left err -> fail $ show err
      Right ts -> mapM_ print ts
  where
    tokens :: Text -> Either SomeException [L T.Token]
    tokens text = P.evalP tokensP (P.emptyPState exts defaultTypes text start)

    start :: Maybe Pos
    start = Just $ startPos filename

    tokensP :: P.P [L T.Token]
    tokensP = do
        t <- L.lexToken
        case t of
          L _ T.Teof  -> return []
          _           -> tokensP >>= \ts -> return (t : ts)

parseFile :: [Flag] -> [V.Extension] -> String -> IO ()
parseFile flags exts filename = do
    text <- E.decodeLatin1 <$> B.readFile filename
    case P.parse exts defaultTypes P.parseDesignFile text start of
      Left err   -> fail $ show err
      Right defs -> if doPrint
                    then if doPragma
                         then putStr $ prettyPragma 80 (ppr defs)
                         else putStr $ pretty 80 (ppr defs)
                    else return ()
  where
    doPrint :: Bool
    doPrint = Print `elem` flags

    doPragma :: Bool
    doPragma = Pragma `elem` flags

    start :: Maybe Pos
    start = Just $ startPos filename
