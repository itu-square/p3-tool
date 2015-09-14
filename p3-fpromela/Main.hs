{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, BangPatterns, ConstraintKinds, FlexibleContexts, TypeFamilies #-}
module Main where

import System.FilePath (splitExtension, (<.>), isValid)
import System.Directory (makeAbsolute)

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Map.Strict as Map
import Data.Foldable (foldrM)
import qualified Data.Set.Monad as Set

import System.Console.CmdLib
import qualified HSH.Command as C
import HSH.ShellEquivs

import Text.Parsec (runParser)

import Language.Preprocessor.Cpphs

import FPromela.Parser as FPromela
import FPromela.Pretty as FPPretty

import qualified Abstraction.Parser as AbsParser
import Abstraction.Ast (Abs(..))

import qualified TVL.Parser as TVL
import qualified TVL.Pretty as TVLPretty

import qualified Transformation.Configurations as Cfgs
import qualified Transformation.Transformation as Trans
import qualified Transformation.Abstraction as Abs

data Main = Main { input :: FilePath, abstraction :: String, output_tvl :: String, output_pml :: String }
  deriving (Typeable, Data, Eq)

instance Attributes Main where
  attributes _ = group "Options" [
    input %> [ Help "Input fPromela file to parse and pretty-print (expects .tvl file with the same name)",
               ArgHelp "INPUT",
               Required True,
               Positional 0],
    abstraction %> [ Help "Abstraction to run, e.g. 'ignore A; project B, !C; join'.\n\
      \  'ignore' takes a comma separated list of feature names which should be ignored\n\
      \  'project' takes a comma separated list of feature literals (A, !A) which it should project\n\
      \ \t [projection on disjunction is not currently supported]\n\
      \  ';' is used to compose multiple abstraction\n\
      \  'join' flattens an fPromela file to Promela by converting all legal 'gd'-statements to 'if'-statements\n\
      \ \t [only makes sense to use as the last abstraction in a composition]",
    ArgHelp "ABSTRACTION",
    Long ["abs"],
    Short ['a'],
    Required False,
    Default "join"],
    output_tvl %> [
      Help "Filename to write output TVL model",
      ArgHelp "TVL_OUTPUT",
      Required False,
      Default "",
      Long ["otvl"],
      Short ['t']
    ],
    output_pml %> [
      Help "Filename to write output fPromela program",
      ArgHelp "PML_OUTPUT",
      Required False,
      Default "",
      Long ["opml"],
      Short ['o']
    ]
   ]

instance RecordCommand Main where
  mode_summary _ = "fPromela file parser and pretty printer"
  run' cmd _ = return ()

type ConcreteMonad = ReaderT (Set.Set Cfgs.Config, [String]) (ExceptT String IO)

runPromela :: FilePath -> [Abs.Abstraction ConcreteMonad] -> String -> String -> IO ()
runPromela file alphas opml otvl = do
  let promela_file = file
  let (fname, ext) = splitExtension file
  let tvl_file = fname <.> "tvl"
  promela_files <- glob promela_file
  when (length promela_files <= 0) $ die ("Cannot find promela file(s): " ++ promela_file)
  tvl_files <- glob tvl_file
  when (length tvl_files <= 0) $ die ("Cannot find TVL file(s): " ++ tvl_file)
  let promela_file_name = head promela_files
  let tvl_file_name = head tvl_files
  promela_file_pre_cpp <- readFile promela_file_name
  promela_file_contents <- runCpphs defaultCpphsOptions { boolopts = defaultBoolOptions { locations = False, lang = False, stripEol = True, stripC89 = True } }
                              promela_file_name promela_file_pre_cpp
  let promela_res = runParser FPromela.pSpec emptyParserState promela_file_name promela_file_contents
  -- This can probably be solved more elegantly using the Either monad
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
             Right cfg -> do
               absout <- runExceptT $ foldrM (\alpha (spec, tvl_res, cfg) -> Trans.abstractSpec alpha spec tvl_res cfg) (promela_res, tvl_res, cfg) alphas
               case absout of
                 Left err -> putStrLn err
                 Right (spec, tvl_res, cfg) -> do
                    if opml == "" || not (isValid opml)
                      then
                        putStrLn . show . FPPretty.prettySpec $ spec
                      else do
                        opml <- makeAbsolute opml
                        putStrLn ("Wrote PML file to " ++ opml)
                        writeFile opml (show . FPPretty.prettySpec $ spec)
                    if otvl == "" || not (isValid otvl)
                      then
                        return ()
                      else do
                        otvl <- makeAbsolute otvl
                        putStrLn ("Wrote TVL file to " ++ otvl)
                        writeFile otvl (show . TVLPretty.prettyModel $ tvl_res)

translateAbs :: (Abs.AbstractionMonad m) => Abs -> Abs.Abstraction m
translateAbs Join = Abs.joinAbs
translateAbs (Ignore fs) = Abs.ignoreAbs $ Set.fromList fs
translateAbs (Project lits) = Abs.projectAbs $ Set.fromList lits

main :: IO ()
main = do
  args <- getArgs
  opts <- executeR (Main { input = "", abstraction = "join", output_pml = "", output_tvl = "" }) args
  let abs_res = runParser AbsParser.pAbstraction () "abstraction" $ abstraction opts
  case abs_res of
    Right alphas -> runPromela (input opts) (Prelude.map translateAbs alphas) (output_pml opts) (output_tvl opts)
    Left err -> putStrLn . ("Error while parsing abstraction: \n " ++) . show $ err
