{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where

import System.Console.CmdLib
import Control.Monad
import qualified HSH.Command as C
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

runPromela :: FilePath -> IO ()
runPromela file = do
  promela_files <- glob file
  when (length promela_files <= 0) $ die ("Cannot find file(s): " ++ file)
  let fname = head promela_files
  promela_file_contents <- C.run $ ("cpp", [fname]) C.-|- ("sed", ["/^\\#/d"])
  let res = runParser pSpec emptyParserState fname promela_file_contents
  case res of
    Left err -> putStrLn . show $ err
    Right res -> putStrLn . show $ res

main :: IO ()
main = do
  args <- getArgs
  opts <- executeR (Main { input = "" }) args
  runPromela . input $ opts
