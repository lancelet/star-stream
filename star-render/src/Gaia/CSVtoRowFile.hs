{-|
Module      : Gaia.CSVtoRowFile
Description : Batch convert Gaia source CSVs to RowFiles.
-}
module Gaia.CSVtoRowFile where

import           Control.Exception.Safe (Exception, MonadThrow, throw)
import           Control.Monad          (foldM, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List              (isPrefixOf, sort)
import           Data.Vector.Unboxed    (Vector)
import qualified Data.Vector.Unboxed    as U
import           Data.Word              (Word64)
import qualified System.Directory       as Dir
import           Text.Printf            (printf)

import qualified Gaia.CSVParser         as CSVParser
import qualified Gaia.RowFile           as RowFile
import qualified Gaia.Types             as GT


newtype RowFileNumber = RowFileNumber Word64


incRowFileNumber :: RowFileNumber -> RowFileNumber
incRowFileNumber (RowFileNumber n) = RowFileNumber (n + 1)


newtype FileExistsError = FileExistsError FilePath deriving Show
instance Exception FileExistsError


-- | Convert a directory full of csv.gz files into row.gz files.
convertCSVFilesToRowFiles
  :: (MonadIO m, MonadThrow m)
  => FilePath  -- ^ Input directory, containing csv.gz files.
  -> FilePath  -- ^ Output directory, to contain row.gz files.
  -> m ()
convertCSVFilesToRowFiles inDir outDir = do

  -- find Gaia CSV files
  inDirContents <- liftIO $ Dir.listDirectory inDir
  let
    gaiaFiles = sort $ filter (isPrefixOf "GaiaSource_") inDirContents
    gaiaPaths = map (\f -> inDir <> "/" <> f) gaiaFiles

  -- process the CSV files; accumulating them into a vector which is written
  -- to a row file whenever its size exceeds 0xFF
  (rowFileNumber, sources) <-
    foldM (csvFileFold outDir) (RowFileNumber 0, U.empty) gaiaPaths
  _ <- writeRowFiles True outDir (rowFileNumber, sources)

  pure ()


-- | Monadic fold function to read CSV files and write row files.
csvFileFold
  :: (MonadIO m, MonadThrow m)
  => FilePath
  -- ^ output directory for row files
  -> (RowFileNumber, Vector GT.ObservedSource)
  -- ^ number of the next row file and currently-accrued sources
  -> FilePath
  -- ^ next CSV file to read
  -> m (RowFileNumber, Vector GT.ObservedSource)
  -- ^ action to write any row files that have been accumulated, returning
  --   the next row file number and the buffer of remaining sources
csvFileFold outDir (nextRowFile, sources') csvFile = do
  liftIO . putStrLn $ "Processing file: " <> csvFile
  sv <- U.fromList <$> CSVParser.readCSVGZFile csvFile
  let sources = sources' <> sv
  writeRowFiles False outDir (nextRowFile, sources)


-- | Function to write row files from accumulated observations.
writeRowFiles
  :: (MonadIO m, MonadThrow m)
  => Bool
  -- ^ True if all remaining sources should be written; False if they
  --   should only be written if more than 0xFF have been accumulated
  -> FilePath
  -- ^ Output director for row files.
  -> (RowFileNumber, Vector GT.ObservedSource)
  -- ^ Next row file number and accumulated sources
  -> m (RowFileNumber, Vector GT.ObservedSource)
writeRowFiles dumpAll outDir (nextRowFile, sources) = do
  let iMax = fromIntegral RowFile.rowFileMaxEntries
  if dumpAll || (U.length sources >= iMax)
    then do
      let
        (sourcesForFile, sources') = U.splitAt iMax sources
        fileName = rowFileName outDir nextRowFile
      fileExists <- liftIO $ Dir.doesPathExist fileName
      when fileExists $ do
        throw . FileExistsError $ fileName
      RowFile.writeRowGZFile fileName sourcesForFile
      if U.length sources' > 0
        then writeRowFiles dumpAll
                           outDir
                           (incRowFileNumber nextRowFile, sources')
        else pure (incRowFileNumber nextRowFile, sources')
    else
      pure (nextRowFile, sources)


-- | Return the name of a row file.
rowFileName :: FilePath -> RowFileNumber -> FilePath
rowFileName outDir (RowFileNumber n) = printf "%s/%.08d.row.gz" outDir n
