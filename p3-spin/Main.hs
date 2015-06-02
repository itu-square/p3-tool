{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, BangPatterns #-}
module Main where

import Control.Monad hiding (forM_)
import Control.Monad.Except hiding (forM_)
import Control.Monad.State hiding (forM_)

import Data.Foldable (forM_)
import Data.Map.Strict as Map
import qualified Data.Set.Monad as Set
import Data.Ratio
import Data.IORef

import Numeric (readFloat, readSigned, fromRat, showFFloat)

import System.Console.CmdLib
import qualified HSH.Command as C
import HSH.ShellEquivs
import Text.Parsec (runParser)

import FPromela.Parser as FPromela
import FPromela.Pretty as FPPretty

import qualified TVL.Parser as TVL
import qualified TVL.Pretty as TVLPretty

import qualified Transformation.Configurations as Cfgs
import qualified Transformation.Transformation as Trans
import qualified Transformation.Abstraction as Abs

import System.IO
import System.IO.Temp
import System.FilePath

import System.Process

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

runWithStdErr :: FilePath -> [String] -> IO (String, String)
runWithStdErr e args = do
  let p = (proc e args) {
      std_out = CreatePipe,
      std_err = CreatePipe
  }
  (_, Just hout, Just herr, ph) <- createProcess p
  !sout <- hGetContents hout
  !serr <- hGetContents herr
  hClose hout
  hClose herr
  !_ <- waitForProcess ph
  return (sout, serr)

runSpin :: FilePath -> IO ()
runSpin file = do
  let promela_file = file ++ ".pml"
  let tvl_file = file ++ ".tvl"
  promela_files <- glob promela_file
  when (length promela_files <= 0) $ die ("Cannot find promela file(s): " ++ promela_file)
  tvl_files <- glob tvl_file
  when (length tvl_files <= 0) $ die ("Cannot find TVL file(s): " ++ tvl_file)
  let promela_file_name = head promela_files
  let tvl_file_name = head tvl_files
  promela_file_contents <- C.run $ ("cpp", ["-w", promela_file_name]) C.-|- ("sed", ["/^\\#/d"])
  let promela_res = runParser FPromela.pSpec emptyParserState promela_file_name promela_file_contents
  case promela_res of
    Left err -> putStrLn . ("Error while parsing promela file(s): \n" ++) . show $ err
    Right promela_res -> do
      tvl_file_contents <- readFile tvl_file_name
      let tvl_res = runParser TVL.pModel () tvl_file_name tvl_file_contents
      case tvl_res of
         Left err -> putStrLn . ("Error while parsing TVL file(s): \n" ++) . show $ err
         Right tvl_res -> do
          cfgs <- runExceptT $ Cfgs.generateConfigs tvl_res
          case cfgs of
             Left err -> putStrLn err
             Right cfgs -> do
               let indcfgs = fmap Set.singleton cfgs
               totalTime <- newIORef (0 :: Rational)
               forM_ indcfgs $ \cfg -> do
                 spec <- runExceptT $ Trans.abstractSpec Abs.joinAbs promela_res cfg
                 case spec of
                   Left err -> putStrLn err
                   Right spec -> do
                     withSystemTempDirectory "p3-spin-" $ \path -> do
                       cd path
                       let specfile = "out.pml"
                       writeFile specfile $ (show . FPPretty.prettySpec) spec
                       !(sout, serr) <- runWithStdErr "/usr/bin/time" ["-p", "spin", "-a", specfile]
                       let ["real", rt , "user", ut, "sys", st] = words serr
                       let spintime = fst . head $ readSigned readFloat ut :: Rational
                       !(sout, serr) <- runWithStdErr "cc" ["-o", "pan", "pan.c", "-DSAFETY", "-O3"]
                       !(sout, serr) <- runWithStdErr "/usr/bin/time" ["-p", "./pan", "-n"]
                       let ["real", rt , "user", ut, "sys", st] = words serr
                       let pantime = fst . head $ readSigned readFloat ut :: Rational
                       modifyIORef' totalTime (spintime + pantime +)
               totalTimeVal <- readIORef totalTime
               putStrLn $ showFFloat (Just 2) (fromRat $ totalTimeVal) ""

main :: IO ()
main = do
  args <- getArgs
  opts <- executeR (Main { input = "" }) args
  runSpin . input $ opts
