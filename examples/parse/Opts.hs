module Opts where

import System.Console.GetOpt
import Data.Maybe (fromMaybe)

data Flag = Tokens
          | Print
          | Pragma
          | Input String
          | Output String
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option []    ["tokens"] (NoArg Tokens)       "show tokens"
    , Option ['p'] ["print"]  (NoArg Print)        "pretty-print file"
    , Option []    ["pragma"] (NoArg Pragma)       "pretty-print with #line pragmas"
    , Option ['o'] ["output"] (OptArg outp "FILE") "output FILE"
    ]

inp,outp :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"
inp  = Input  . fromMaybe "stdin"

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute options argv of
      { (o,n,[]  ) -> return (o,n)
      ; (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      }
  where
    header = "Usage: parse [OPTION...] files..."
