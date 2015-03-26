{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where

import Control.Monad
import Control.Monad.Error


import System.Console.CmdLib
import qualified HSH.Command as C
import HSH.ShellEquivs
import Text.Parsec (runParser)

import FPromela.Parser as FPromela
import FPromela.Pretty as FPPretty

import qualified TVL.Parser as TVL

import qualified Transformation.Configurations as Cfgs
import qualified Transformation.Transformation as Trans
import qualified Transformation.Abstraction as Abs

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
  run' cmd _ = return ()

runPromela :: FilePath -> IO ()
runPromela file = do
  let promela_file = file ++ ".pml"
  let tvl_file = file ++ ".tvl"
  promela_files <- glob promela_file
  when (length promela_files <= 0) $ die ("Cannot find promela file(s): " ++ promela_file)
  tvl_files <- glob tvl_file
  when (length tvl_files <= 0) $ die ("Cannot find TVL file(s): " ++ tvl_file)
  let promela_file_name = head promela_files
  let tvl_file_name = head tvl_files
  promela_file_contents <- C.run $ ("cpp", [promela_file_name]) C.-|- ("sed", ["/^\\#/d"])
  let promela_res = runParser FPromela.pSpec emptyParserState promela_file_name promela_file_contents
  case promela_res of
    Left err -> putStrLn . ("Error while parsing promela file(s): \n" ++) . show $ err
    Right promela_res -> do
      tvl_file_contents <- readFile tvl_file_name
      let tvl_res = runParser TVL.pModel () tvl_file_name tvl_file_contents
      case tvl_res of
         Left err -> putStrLn . ("Error while parsing TVL file(s): \n" ++) . show $ err
         Right tvl_res -> do
          cfgs <- runErrorT $ Cfgs.generateConfigs tvl_res
          case cfgs of
             Left err -> putStrLn err
             Right cfg -> do
               spec <- runErrorT $ Trans.abstractSpec cfg Abs.joinAbs promela_res
               putStrLn . show $ spec

main :: IO ()
main = do
  args <- getArgs
  opts <- executeR (Main { input = "" }) args
  runPromela . input $ opts
