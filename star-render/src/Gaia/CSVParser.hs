{-|
Module      : Gaia.CSVParser
Description : Specialized parser for Gaia CSV files.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module Gaia.CSVParser
  ( -- * Types
    ParseError(ParseError)
    -- * Functions
  , readCSVGZFile
  , readCSVFile
  ) where

import qualified Codec.Compression.GZip           as GZip
import           Control.Applicative              ((<|>))
import           Control.Exception.Safe           (Exception, MonadThrow, throw)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Attoparsec.ByteString.Char8 (Parser, char, count, double,
                                                   endOfInput, endOfLine,
                                                   option, parseOnly, sepBy1,
                                                   skipWhile, string, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as Atto (decimal)
import qualified Data.ByteString.Char8            as BS8 (intercalate, readFile)
import qualified Data.ByteString.Lazy             as LBS
import           Data.Text                        (Text)
import qualified Data.Text                        as Text (pack)
import           Data.Word                        (Word32, Word64)

import qualified Gaia.Types                       as GT

newtype ParseError = ParseError Text deriving stock Show
instance Exception ParseError


-- | Read a GZipped CSV file containing Gaia DR2 observed sources.
--
-- Errors encountered when parsing ('ParseError') will be thrown as exceptions.
readCSVGZFile
  :: (MonadIO m, MonadThrow m)
  => FilePath
  -> m [GT.ObservedSource]
readCSVGZFile inFile = do
  result <- readCSVGZFile' inFile
  case result of
    Left err      -> throw err
    Right sources -> pure sources


-- | Read a GZipped CSV file containing Gaia DR2 observed sources.
--
-- Parse errors are returned as a value in the result.
readCSVGZFile'
  :: MonadIO m
  => FilePath
  -> m (Either ParseError [GT.ObservedSource])
readCSVGZFile' inFile = do
  bsz <- liftIO $ LBS.readFile inFile
  let bs = LBS.toStrict . GZip.decompress $ bsz
  case parseOnly parseCSVFile bs of
    Left err      -> pure . Left . ParseError . Text.pack $ err
    Right sources -> pure . Right $ sources


-- | Read a CSV file containing Gaia DR2 observed sources.
--
-- Errors encountered when parsing ('ParseError') will be thrown as exceptions.
readCSVFile
  :: (MonadIO m, MonadThrow m)
  => FilePath               -- ^ Input file path.
  -> m [GT.ObservedSource]  -- ^ Produced list of observations.
readCSVFile inFile = do
  result <- readCSVFile' inFile
  case result of
    Left err      -> throw err
    Right sources -> pure sources


-- | Read a CSV file containing Gaia DR2 observed sources.
--
-- Parse errors are returned as a value in the result.
readCSVFile'
  :: MonadIO m
  => FilePath
  -> m (Either ParseError [GT.ObservedSource])
readCSVFile' inFile = do
  bs <- liftIO $ BS8.readFile inFile
  case parseOnly parseCSVFile bs of
    Left err      -> pure . Left . ParseError . Text.pack $ err
    Right sources -> pure . Right $ sources


parseCSVFile :: Parser [GT.ObservedSource]
parseCSVFile
  = skipHeaderLine
  *> sepBy1 parseObservedSource endOfLine
  <* (option () endOfLine)
  <* endOfInput


parseObservedSource :: Parser GT.ObservedSource
parseObservedSource = do

  -- parse identifying information about the observed source
  solutionId <- parseSolutionId
  skipField
  sourceId <- parseSourceId
  randomIndex <- parseRandomIndex
  let identifier = GT.Id solutionId sourceId randomIndex

  -- parse position information: RA and Dec
  skipField
  ra <- parseRA
  skipField
  dec <- parseDec
  skipField
  let position = GT.Position ra dec

  -- skip most of the data
  skipNFields 38

  -- parse available photometric data
  meanFluxG <- parseMeanFluxG
  skipNFields 3
  bNObs <- word32 <* skipSeparator
  meanFluxBP <-
    if bNObs > 0
    then parseMeanFluxBP
    else skipField *> pure GT.NoBPBand
  skipNFields 3
  rNObs <- word32 <* skipSeparator
  meanFluxRP <-
    if rNObs > 0
    then parseMeanFluxRP
    else skipField *> pure GT.NoRPBand
  let photometry = GT.Photometry meanFluxG meanFluxBP meanFluxRP

  -- skip the remainder of the line
  skipRestOfLine

  pure $ GT.ObservedSource identifier position photometry


parseSolutionId :: Parser GT.SolutionId
parseSolutionId
  = (GT.SolutionId <$> word64 <* skipSeparator)
    <?> "SolutionId"
{-# INLINE parseSolutionId #-}


parseSourceId :: Parser GT.SourceId
parseSourceId
  = (GT.SourceId <$> word64 <* skipSeparator)
    <?> "SourceId"
{-# INLINE parseSourceId #-}


parseRandomIndex :: Parser GT.RandomIndex
parseRandomIndex
  = (GT.RandomIndex <$> word64 <* skipSeparator)
    <?> "RandomIndex"
{-# INLINE parseRandomIndex #-}


parseRA :: Parser GT.RA
parseRA
  = (GT.RA <$> double <* skipSeparator)
    <?> "RA"
{-# INLINE parseRA #-}


parseDec :: Parser GT.Dec
parseDec
  = (GT.Dec <$> double <* skipSeparator)
    <?> "Dec"
{-# INLINE parseDec #-}


parseMeanFluxG :: Parser GT.MeanFluxG
parseMeanFluxG
  = ((GT.MeanFluxG . GT.MeanFlux) <$> double <* skipSeparator)
    <?> "MeanFluxG"
{-# INLINE parseMeanFluxG #-}


parseMeanFluxBP :: Parser GT.MeanFluxBP
parseMeanFluxBP
  = ((GT.MeanFluxBP . GT.MeanFlux) <$> double <* skipSeparator)
    <?> "MeanFluxBP"
{-# INLINE parseMeanFluxBP #-}


parseMeanFluxRP :: Parser GT.MeanFluxRP
parseMeanFluxRP
  = ((GT.MeanFluxRP . GT.MeanFlux) <$> double <* skipSeparator)
    <?> "MeanFluxRP"
{-# INLINE parseMeanFluxRP #-}


word32 :: Parser Word32
word32 = Atto.decimal <?> "Word32"
{-# INLINE word32 #-}


word64 :: Parser Word64
word64 = Atto.decimal <?> "Word64"
{-# INLINE word64 #-}


skipSeparator :: Parser ()
skipSeparator = skipComma <|> endOfLine <|> endOfInput
  where
    skipComma = char ',' *> pure ()
{-# INLINE skipSeparator #-}


skipNFields :: Int -> Parser ()
skipNFields n = (count n skipField *> pure ()) <?> ("skipNFields " <> show n)
{-# INLINE skipNFields #-}


skipField :: Parser ()
skipField =
  let
    separator c =
      case c of
        ','  -> True
        '\n' -> True
        '\r' -> True
        _    -> False
  in
    (skipWhile (not . separator) *> skipSeparator) <?> "skipField"
{-# INLINE skipField #-}


skipRestOfLine :: Parser ()
skipRestOfLine =
  let
    eol c =
      case c of
        '\n' -> True
        '\r' -> True
        _    -> False
  in
    skipWhile (not . eol) <?> "skipRestOfLine"
{-# INLINE skipRestOfLine #-}


skipHeaderLine :: Parser ()
skipHeaderLine
  = string
    (BS8.intercalate ","
     [ "solution_id"
     , "designation"
     , "source_id"
     , "random_index"
     , "ref_epoch"
     , "ra"
     , "ra_error"
     , "dec"
     , "dec_error"
     , "parallax"
     , "parallax_error"
     , "parallax_over_error"
     , "pmra,pmra_error"
     , "pmdec,pmdec_error"
     , "ra_dec_corr"
     , "ra_parallax_corr"
     , "ra_pmra_corr"
     , "ra_pmdec_corr"
     , "dec_parallax_corr"
     , "dec_pmra_corr"
     , "dec_pmdec_corr"
     , "parallax_pmra_corr"
     , "parallax_pmdec_corr"
     , "pmra_pmdec_corr"
     , "astrometric_n_obs_al"
     , "astrometric_n_obs_ac"
     , "astrometric_n_good_obs_al"
     , "astrometric_n_bad_obs_al"
     , "astrometric_gof_al"
     , "astrometric_chi2_al"
     , "astrometric_excess_noise"
     , "astrometric_excess_noise_sig"
     , "astrometric_params_solved"
     , "astrometric_primary_flag"
     , "astrometric_weight_al"
     , "astrometric_pseudo_colour"
     , "astrometric_pseudo_colour_error"
     , "mean_varpi_factor_al"
     , "astrometric_matched_observations"
     , "visibility_periods_used"
     , "astrometric_sigma5d_max"
     , "frame_rotator_object_type"
     , "matched_observations,duplicated_source"
     , "phot_g_n_obs"
     , "phot_g_mean_flux"
     , "phot_g_mean_flux_error"
     , "phot_g_mean_flux_over_error"
     , "phot_g_mean_mag"
     , "phot_bp_n_obs"
     , "phot_bp_mean_flux"
     , "phot_bp_mean_flux_error"
     , "phot_bp_mean_flux_over_error"
     , "phot_bp_mean_mag"
     , "phot_rp_n_obs,phot_rp_mean_flux"
     , "phot_rp_mean_flux_error"
     , "phot_rp_mean_flux_over_error"
     , "phot_rp_mean_mag"
     , "phot_bp_rp_excess_factor"
     , "phot_proc_mode"
     , "bp_rp"
     , "bp_g"
     , "g_rp"
     , "radial_velocity"
     , "radial_velocity_error"
     , "rv_nb_transits"
     , "rv_template_teff"
     , "rv_template_logg"
     , "rv_template_fe_h"
     , "phot_variable_flag"
     , "l"
     , "b"
     , "ecl_lon"
     , "ecl_lat"
     , "priam_flags"
     , "teff_val"
     , "teff_percentile_lower"
     , "teff_percentile_upper"
     , "a_g_val"
     , "a_g_percentile_lower"
     , "a_g_percentile_upper"
     , "e_bp_min_rp_val"
     , "e_bp_min_rp_percentile_lower"
     , "e_bp_min_rp_percentile_upper"
     , "flame_flags"
     , "radius_val"
     , "radius_percentile_lower"
     , "radius_percentile_upper"
     , "lum_val"
     , "lum_percentile_lower"
     , "lum_percentile_upper"
     ]
    )
  *> endOfLine
  *> pure ()
  <?> "header line"
