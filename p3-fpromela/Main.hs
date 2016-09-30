{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, BangPatterns, ConstraintKinds, FlexibleContexts, TypeFamilies #-}
module Main where

import System.FilePath ((-<.>), (</>), isAbsolute, isValid)
import System.Directory (makeAbsolute, getCurrentDirectory)

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Maybe (fromMaybe)
import Data.Map.Strict as Map
import Data.Foldable (foldlM)
import qualified Data.Set.Monad as Set
import Data.Semigroup

import Text.Parsec (runParser)

import Language.Preprocessor.Cpphs

import FPromela.Parser as FPromela
import FPromela.Pretty as FPPretty

import qualified Abstraction.Parser as AbsParser
import Abstraction.Ast (Abs(..))

import qualified TVL.Parser as TVL
import qualified TVL.Pretty as TVLPretty

import qualified Transformation.Configurations as Cfgs
import qualified Transformation.Formulae as Frm
import qualified Transformation.Transformation as Trans
import qualified Transformation.Abstraction as Abs

import qualified Options.Applicative as Opt

data ToolOpts = ToolOpts { input :: FilePath, abstraction :: Maybe String, output_tvl :: Maybe String, output_pml :: Maybe String }
  deriving Eq

toolOpts :: Opt.Parser ToolOpts
toolOpts = ToolOpts
  <$> Opt.argument Opt.str
       (  Opt.metavar "INPUT"
       <> Opt.help "Input fPromela file to abstract (expects .tvl file with the same name)"
       )
  <*> Opt.optional (Opt.strOption
      (
         Opt.long "abs"
      <> Opt.short 'a'
      <> Opt.metavar "ABSTRACTION"
      <> Opt.help "Abstraction to run, e.g. 'ignore A; project B, !C; join'.\n\
      \  'ignore' takes a comma separated list of feature names which should be ignored\n\
      \  'project' takes a comma separated list of feature literals (A, !A) which it should project\n\
      \ \t [projection on disjunction is not currently supported]\n\
      \  ';' is used to compose multiple abstraction\n\
      \  'join' flattens an fPromela file to Promela by converting all legal 'gd'-statements to 'if'-statements\n\
      \ \t [only makes sense to use as the last abstraction in a composition]"
      ))
  <*> Opt.optional (Opt.strOption
      (  Opt.long "otvl"
      <> Opt.short 't'
      <> Opt.metavar "TVL_OUTPUT"
      <> Opt.help "Filename to write output TVL model"
      ))
  <*> Opt.optional (Opt.strOption
      (  Opt.long "opml"
      <> Opt.short 'o'
      <> Opt.metavar "PML_OUTPUT"
      <> Opt.help "Filename to write output fPromela program"
      ))

type ConcreteMonad = ReaderT (Frm.Formula, [String]) (ExceptT String IO)

runPromela :: FilePath -> [Abs.Abstraction ConcreteMonad] -> Maybe String -> Maybe String -> IO ()
runPromela file alphas opml otvl = do
  currentDir <- getCurrentDirectory
  let promela_file = if isAbsolute file then file else currentDir </> file
  let tvl_file = promela_file -<.> "tvl"
  promela_file_pre_cpp <- readFile promela_file
  promela_file_contents <- runCpphs defaultCpphsOptions { boolopts = defaultBoolOptions { locations = False, lang = False, stripEol = True, stripC89 = True } }
                              promela_file promela_file_pre_cpp
  let promela_res = runParser FPromela.pSpec emptyParserState promela_file promela_file_contents
  -- This can probably be solved more elegantly using the Either monad
  case promela_res of
    Left err -> putStrLn . ("Error while parsing promela file(s): \n" ++) . show $ err
    Right promela_res -> do
      tvl_file_contents <- readFile tvl_file
      let tvl_res = runParser TVL.pModel () tvl_file tvl_file_contents
      case tvl_res of
         Left err -> putStrLn . ("Error while parsing TVL file(s): \n" ++) . show $ err
         Right tvl_res -> do
          cfgs <- runExceptT $ Cfgs.generateConfigs tvl_res
          case cfgs of
             Left err -> putStrLn err
             Right cfg -> do
               absout <- runExceptT $ foldlM (\(spec, tvl_res, cfg) alpha -> Trans.abstractSpec alpha spec tvl_res cfg) (promela_res, tvl_res, cfg) alphas
               case absout of
                 Left err -> putStrLn err
                 Right (spec, tvl_res, cfg) -> do
                    case opml of
                      Just opmlPath | isValid opmlPath -> do
                        opml <- makeAbsolute opmlPath
                        putStrLn ("Wrote PML file to " ++ opml)
                        writeFile opml (show . FPPretty.prettySpec $ spec)
                      _ -> print . FPPretty.prettySpec $ spec
                    case otvl of
                      Just otvlPath | isValid otvlPath -> do
                        otvl <- makeAbsolute otvlPath
                        putStrLn ("Wrote TVL file to " ++ otvl)
                        writeFile otvl (show . TVLPretty.prettyModel $ tvl_res)
                      _ -> return ()

translateAbs :: (Abs.AbstractionMonad m) => Abs -> Abs.Abstraction m
translateAbs (Join neg) = Abs.joinAbs neg
translateAbs (Ignore neg fs) = Abs.ignoreAbs neg $ Set.fromList fs
translateAbs (Project lits) = Abs.projectAbs $ Set.fromList lits

main :: IO ()
main = do
    opts <- Opt.execParser optParser
    let abs_res = runParser AbsParser.pAbstraction () "abstraction" $ fromMaybe "join" (abstraction opts)
    case abs_res of
      Right alphas -> runPromela (input opts) (Prelude.map translateAbs alphas) (output_pml opts) (output_tvl opts)
      Left err -> putStrLn . ("Error while parsing abstraction: \n " ++) . show $ err
  where optParser = Opt.info (Opt.helper <*> toolOpts)
            (  Opt.fullDesc
            <> Opt.progDesc "Apply ABSTRACTION to INPUT producing TVL_OUTPUT and PML_OUTPUT"
            <> Opt.header "p3-fpromela - a tool for applying variability abstractions to fPromela models")
