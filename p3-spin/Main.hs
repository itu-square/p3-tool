{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}
module Main where

import Control.Monad hiding (forM_)
import Control.Monad.Except hiding (forM_)
import Control.Monad.State hiding (forM_)

import Data.Semigroup
import Data.Foldable (forM_)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set.Monad as Set
import Data.Ratio
import Data.IORef

import Numeric (readFloat, readSigned, fromRat, showFFloat)

import Text.Parsec (runParser)

import FPromela.Parser as FPromela
import FPromela.Pretty as FPPretty

import qualified TVL.Parser as TVL
import qualified TVL.Pretty as TVLPretty

import qualified Transformation.Formulae as Frm
import qualified Transformation.Configurations as Cfgs
import qualified Transformation.Transformation as Trans
import qualified Transformation.Abstraction as Abs

import Language.Preprocessor.Cpphs

import System.IO
import System.IO.Temp
import System.FilePath
import System.Directory

import System.Process

import qualified Options.Applicative as Opt

data ToolOpts = ToolOpts { input :: FilePath }
  deriving Eq

toolOpts :: Opt.Parser ToolOpts
toolOpts = ToolOpts <$> Opt.argument Opt.str
                         (  Opt.metavar "INPUT"
                         <> Opt.help "Input fPromela file to verify with SPIN"
                         )

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
  currentDir <- getCurrentDirectory
  let promela_file = if isAbsolute file then file else currentDir </> file
  let tvl_file = file -<.> ".tvl"
  promela_file_pre_cpp <- readFile promela_file
  promela_file_contents <- runCpphs defaultCpphsOptions { boolopts = defaultBoolOptions { locations = False, lang = False, stripEol = True, stripC89 = True } } promela_file promela_file_pre_cpp
  let promela_res = runParser FPromela.pSpec emptyParserState promela_file promela_file_contents
  case promela_res of
    Left err -> putStrLn . ("Error while parsing promela file(s): \n" ++) . show $ err
    Right promela_res -> do
      tvl_file_contents <- readFile tvl_file
      let tvl_res = runParser TVL.pModel () tvl_file tvl_file_contents
      case tvl_res of
         Left err -> putStrLn . ("Error while parsing TVL file(s): \n" ++) . show $ err
         Right tvl_res -> do
          cfgsfrmE <- runExceptT $ Cfgs.generateConfigs tvl_res
          case cfgsfrmE of
             Left err -> putStrLn err
             Right cfgsfrm -> do
               indcfgs <- Frm.allSatisfiable cfgsfrm
               totalTime <- newIORef (0 :: Rational)
               forM_ indcfgs $ \cfg -> do
                 let cfgfrm = Frm.fAll $ map (\(var, val) ->
                                                if val then Frm.FVar var
                                                else (Frm.:!:) (Frm.FVar var)) (Map.toList cfg)
                 spec <- runExceptT $ Trans.abstractSpec (Abs.joinAbs False) promela_res tvl_res cfgfrm
                 case spec of
                   Left err -> putStrLn err
                   Right (spec, tvl_res, cfg) ->
                     withSystemTempDirectory "p3-spin-" $ \path -> do
                       setCurrentDirectory path
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
               putStrLn $ showFFloat (Just 2) (fromRat totalTimeVal) ""

main :: IO ()
main = do
    opts <- Opt.execParser optParser
    runSpin . input $ opts
  where optParser =  Opt.info (Opt.helper <*> toolOpts)
          (  Opt.fullDesc
          <> Opt.progDesc "Verify INPUT model with Spin"
          <> Opt.header "p3-spin - a tool for bruteforce verification of fPromela models with SPIN")

