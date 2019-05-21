{-|
Module      : Main
Description : Main module for the gaia-fetch downloader.
-}
module Main where

import qualified GaiaFetch           as GF

import           Control.Monad       (when)
import qualified Options.Applicative as OA
import qualified System.Directory    as Directory

main :: IO ()
main = do
  let
    opts = OA.info (parseConfig OA.<**> OA.helper)
           ( OA.fullDesc
           <> OA.progDesc "Download Gaia mission star catalog to DIR"
           <> OA.header ("gaia-fetch - a utility to download the Gaia Mission "
                         <> "star catalog") )
  config <- OA.execParser opts

  configOK <- validateConfig config
  when configOK $ do
    env <- GF.defaultEnv
    let gaiaLocalDir = GF.GaiaLocalDir . cfgLocalDirectory $ config
    GF.download' env gaiaLocalDir


-- | Command-line configuration.
data Config
  = Config
    { cfgLocalDirectory :: FilePath
    }


-- | Validate the command-line configuration.
--
-- Return `True` if the config is OK.
validateConfig :: Config -> IO Bool
validateConfig config = do
  let dir = cfgLocalDirectory config
  dirExists <- Directory.doesDirectoryExist dir
  when (not dirExists) $ putStrLn $ dir <> " must be an existing directory"
  pure $ dirExists


-- | Parse command-line configuration.
parseConfig :: OA.Parser Config
parseConfig = Config <$> (OA.argument OA.str (OA.metavar "DIR"))
