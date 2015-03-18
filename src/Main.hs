{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where

import System.Console.CmdLib
import Control.Monad
import HSH.ShellEquivs
import Text.Parsec (runParser)

import FPromela.Parser
import FPromela.Ast

data Main = Main { input :: FilePath }
  deriving (Typeable, Data, Eq)

instance Attributes Main where
  attributes _ = group "Options" [
    input %> [ Help "Input fPromela file to parse and pretty-print",
               ArgHelp "FILENAME",
               Required True ]
   ]

instance RecordCommand Main where
  mode_summary _ = "fPromela file parser and pretty printer"

main :: IO ()
main = do
  args <- getArgs
  opts <- executeR (Main { input = "" }) args
  promela_files <- glob . input $ opts
  when (length promela_files <= 0) $ die ("Cannot find file(s): " ++ input opts)
  let fname = head promela_files
  promela_file_contents <- readFile fname
  let res = runParser pSpec emptyParserState fname promela_file_contents
  case res of
    Left err -> putStrLn . show $ err
    Right _ -> putStrLn "Fini"
