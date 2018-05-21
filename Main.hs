module Main where

import Prelude hiding (abs)
import Data.Text as T (unpack, stripEnd, pack, split)
import Control.Monad
import System.Exit
import System.Environment
import ULC

rstrip = T.unpack . T.stripEnd . T.pack
splitStr c s = map T.unpack xs
  where
    xs = T.split ((==)c) (T.pack s)

data Term =
    TmVar  String
  | TmAbs  String Term
  | TmApp  Term Term
  | TmReset  Term
  | TmCallCC  String Term
  | TmBinOp BinOpSort Term Term
  | TmConst Int
  deriving (Show)

ops :: Ops Term
ops = TermConstruction
              { abs  = const TmAbs
              , var  = const TmVar
              , app  = const TmApp
              , reset = const TmReset
              , call  = const TmCallCC
              , int   = const TmConst
              , binop = TmBinOp
              }

main :: IO ()
main = do
  args <- System.Environment.getArgs
  text <-
    case args of
      [file_name] -> rstrip <$> readFile file_name
      _      -> putStrLn "File not specified" >> exitFailure
  let lines = splitStr '\n' text

  mapM_ (\line -> print line >> print (ULC.parse ops line)) lines
  exitSuccess
