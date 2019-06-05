{-|
Module      : Main
Description : Main file for the binarize binary.

Binarize converts text-based csv.gz files into binary row.gz files. The binary
files are faster to process and smaller.
-}
module Main (main) where

import           Control.Monad     (when)
import qualified Options.Applicative as OA
import qualified System.Directory  as Directory

import           Gaia.CSVtoRowFile (convertCSVFilesToRowFiles)


main :: IO ()
main = do
  let
    opts
      = OA.info (parseConfig OA.<**> OA.helper)
        ( OA.fullDesc
          <> OA.progDesc "Convert Gaia csv.gz files into binary row.gz files"
          <> OA.header ( "binarize - a utility to convert Gaia csv.gz files "
                         <> "into smaller, simpler binary row.gz files" ) )
  config <- OA.execParser opts

  configOK <- validateConfig config
  when configOK $ 
    convertCSVFilesToRowFiles (cfgInDir config) (cfgOutDir config)


data Config
  = Config
    { cfgInDir  :: FilePath
    , cfgOutDir :: FilePath
    }


validateConfig :: Config -> IO Bool
validateConfig config = do
  inDirExists <- Directory.doesDirectoryExist (cfgInDir config)
  when (not inDirExists) $
    putStrLn $ (cfgInDir config) <> " must be an existing directory"

  outDirExists <- Directory.doesDirectoryExist (cfgOutDir config)
  when (not outDirExists) $
    putStrLn $ (cfgOutDir config) <> " must be an existing directory"

  pure (inDirExists && outDirExists)


parseConfig :: OA.Parser Config
parseConfig
  = Config
    <$> (OA.argument OA.str (OA.metavar "IN_DIR"))
    <*> (OA.argument OA.str (OA.metavar "OUT_DIR"))
