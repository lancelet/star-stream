{-|
Module      : Gaia.RowFile
Description : Storing Gaia data in files with rows.

Row files are a binary format for storing Gaia data for rendering. They consist
of a header and body containing multiple records. Up to 1048576 (0xFFFFF + 1)
records can be stored in a single row file.

The format is little-endian Intel, as follows:

+--------+---------------+--------------------------------------------------+
| Part   | Format        | Description                                      |
+========+===============+==================================================+
| Header | 7-bytes ASCII | "GAIAROW" magic number                           |
|        +---------------+--------------------------------------------------+
|        | Word32        | Number of records in file minus 1 (max 1048575)  |
+--------+---------------+--------------------------------------------------+
| Body   | Word64        | Solution Id                                      |
|        +---------------+--------------------------------------------------+
|        | Word64        | Source Id                                        |
|        +---------------+--------------------------------------------------+
|        | Word64        | Random Index                                     |
|        +---------------+--------------------------------------------------+
|        | Double        | Right Ascension                                  |
|        +---------------+--------------------------------------------------+
|        | Double        | Declination                                      |
|        +---------------+--------------------------------------------------+
|        | Byte          | Photometry Status (see below)                    |
|        +---------------+--------------------------------------------------+
|        | Double        | Mean Flux G                                      |
|        +---------------+--------------------------------------------------+
|        | Double        | Mean Flux BP (or zero)                           |
|        +---------------+--------------------------------------------------+
|        | Double        | Mean Flux RP (or zero)                           |
|        +---------------+--------------------------------------------------+
|        | ...           | ...                                              |
+--------+---------------+--------------------------------------------------+

Only one record in the body is shown above, but the pattern repeats for
multiple records.

The photometry status byte indicates whether the BP and RP flux channels
are available. The bits of this byte are set as follows:

  X X X X X X RP BP

where 'X' indicates a bit that is not set, 'RP' indicates that the RP
channel is avalable, and 'BP' indicates that the BP channel is available.

The files can be read/written in full, in both plain and GZipped formats.
Additionally, records can be appended to the end of one of these files
provided that there is still space.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module Gaia.RowFile
  ( -- * Types
    ParseError(ParseError)
    -- * Functions
  , rowFileMaxEntries
    -- ** Whole-file IO
  , readRowFile
  , readRowGZFile
  , writeRowFile
  , writeRowGZFile
  ) where

import qualified Codec.Compression.GZip as GZip
import           Control.Exception.Safe (Exception, MonadThrow, throw)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Binary.Get        (Get)
import           Data.Binary.Get        as Get (Decoder (Done, Fail, Partial),
                                                getByteString, getDoublele,
                                                getWord32le, getWord64le,
                                                getWord8, pushChunk,
                                                pushEndOfInput,
                                                runGetIncremental)
import           Data.Binary.Put        (Put)
import           Data.Binary.Put        as Put (putByteString, putDoublele,
                                                putWord32le, putWord64le,
                                                putWord8, runPut)
import           Data.Bits              (bit, testBit, (.|.))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Vector.Unboxed    (Vector)
import qualified Data.Vector.Unboxed    as U
import           Data.Word              (Word32, Word8)

import qualified Gaia.Types             as GT


-- Whole-file IO --------------------------------------------------------------


-- | Maximum number of entries in a row file.
rowFileMaxEntries :: Word32
rowFileMaxEntries = 0x000FFFFF + 1


newtype ParseError = ParseError Text deriving stock Show
instance Exception ParseError


readRowGZFile
  :: (MonadIO m, MonadThrow m)
  => FilePath
  -> m (Vector GT.ObservedSource)
readRowGZFile inFile = do
  bsz <- liftIO $ LBS.readFile inFile
  let
    bs = LBS.toStrict . GZip.decompress $ bsz
    result = pushEndOfInput $ runGetIncremental getRowFile `pushChunk` bs
  case result of
    Fail _ _ msg -> throw . ParseError . Text.pack $ msg
    Partial _    -> throw . ParseError $ "readRowFile: Partial input!"
    Done _ _ sv  -> pure sv


readRowFile
  :: (MonadIO m, MonadThrow m)
  => FilePath
  -> m (Vector GT.ObservedSource)
readRowFile inFile = do
  bs <- liftIO $ BS.readFile inFile
  let
    result = pushEndOfInput $ runGetIncremental getRowFile `pushChunk` bs
  case result of
    Fail _ _ msg -> throw . ParseError . Text.pack $ msg
    Partial _    -> throw . ParseError $ "readRowFile: Partial input!"
    Done _ _ sv  -> pure sv


writeRowGZFile
  :: MonadIO m
  => FilePath
  -> Vector GT.ObservedSource
  -> m ()
writeRowGZFile outFile sv = do
  let
    lbs = runPut $ putRowFile sv
    params = GZip.defaultCompressParams
             { GZip.compressLevel       = GZip.bestCompression
             , GZip.compressMemoryLevel = GZip.maxMemoryLevel
             }
    lbsz = GZip.compressWith params lbs
  liftIO $ LBS.writeFile outFile lbsz


writeRowFile
  :: MonadIO m
  => FilePath
  -> Vector GT.ObservedSource
  -> m ()
writeRowFile outFile sv = do
  let lbs = runPut $ putRowFile sv
  liftIO $ LBS.writeFile outFile lbs


-- Get Observations -----------------------------------------------------------

getRowFile :: Get (Vector GT.ObservedSource)
getRowFile = do
  magic <- getByteString 7
  if magic /= "GAIAROW"
  then fail $ "getRowFile was expecting GAIAROW as a file identifier"
  else do
    len <- (((+) 1) . fromIntegral) <$> getWord32le
    U.replicateM len getObservedSource

getObservedSource :: Get GT.ObservedSource
getObservedSource
  = GT.ObservedSource
    <$> getId
    <*> getPosition
    <*> getPhotometry
{-# INLINE getObservedSource #-}

getId :: Get GT.Id
getId
  = GT.Id
    <$> getSolutionId
    <*> getSourceId
    <*> getRandomIndex
{-# INLINE getId #-}

getSolutionId :: Get GT.SolutionId
getSolutionId = GT.SolutionId <$> getWord64le
{-# INLINE getSolutionId #-}

getSourceId :: Get GT.SourceId
getSourceId = GT.SourceId <$> getWord64le
{-# INLINE getSourceId #-}

getRandomIndex :: Get GT.RandomIndex
getRandomIndex = GT.RandomIndex <$> getWord64le
{-# INLINE getRandomIndex #-}

getPosition :: Get GT.Position
getPosition
  = GT.Position
    <$> getRA
    <*> getDec
{-# INLINE getPosition #-}

getRA :: Get GT.RA
getRA = GT.RA <$> getDoublele
{-# INLINE getRA #-}

getDec :: Get GT.Dec
getDec = GT.Dec <$> getDoublele
{-# INLINE getDec #-}

getPhotometry :: Get GT.Photometry
getPhotometry = do
  status <- decodePhotometryStatus <$> getWord8
  GT.Photometry
    <$> getMeanFluxG
    <*> getMeanFluxBP status
    <*> getMeanFluxRP status
{-# INLINE getPhotometry #-}

getMeanFlux :: Get GT.MeanFlux
getMeanFlux = GT.MeanFlux <$> getDoublele
{-# INLINE getMeanFlux #-}

getMeanFluxG :: Get GT.MeanFluxG
getMeanFluxG = GT.MeanFluxG <$> getMeanFlux
{-# INLINE getMeanFluxG #-}

getMeanFluxBP :: PhotometryStatus -> Get GT.MeanFluxBP
getMeanFluxBP status
  = if psHasMeanFluxBP status
    then GT.MeanFluxBP <$> getMeanFlux
    else getMeanFlux >> pure GT.NoBPBand
{-# INLINE getMeanFluxBP #-}

getMeanFluxRP :: PhotometryStatus -> Get GT.MeanFluxRP
getMeanFluxRP status
  = if psHasMeanFluxRP status
    then GT.MeanFluxRP <$> getMeanFlux
    else getMeanFlux >> pure GT.NoRPBand
{-# INLINE getMeanFluxRP #-}

-- Put Observations -----------------------------------------------------------

putRowFile :: Vector GT.ObservedSource -> Put
putRowFile sv = do
  let len = U.length sv
  if len > fromIntegral rowFileMaxEntries
  then fail $ "putRowFile requires a vector with <= "
           <> show rowFileMaxEntries
           <> " elements"
  else do
    putByteString "GAIAROW"
    putWord32le (fromIntegral (len - 1))
    U.mapM_ putObservedSource sv

putObservedSource :: GT.ObservedSource -> Put
putObservedSource (GT.ObservedSource identifier position photometry)
  =  putId identifier
  >> putPosition position
  >> putPhotometry photometry
{-# INLINE putObservedSource #-}

putId :: GT.Id -> Put
putId (GT.Id solutionId sourceId randomIndex)
  =  putSolutionId solutionId
  >> putSourceId sourceId
  >> putRandomIndex randomIndex
{-# INLINE putId #-}

putSolutionId :: GT.SolutionId -> Put
putSolutionId (GT.SolutionId i) = putWord64le i
{-# INLINE putSolutionId #-}

putSourceId :: GT.SourceId -> Put
putSourceId (GT.SourceId i) = putWord64le i
{-# INLINE putSourceId #-}

putRandomIndex :: GT.RandomIndex -> Put
putRandomIndex (GT.RandomIndex i) = putWord64le i
{-# INLINE putRandomIndex #-}

putPosition :: GT.Position -> Put
putPosition (GT.Position ra dec)
  =  putRA ra
  >> putDec dec
{-# INLINE putPosition #-}

putRA :: GT.RA -> Put
putRA (GT.RA x) = putDoublele x
{-# INLINE putRA #-}

putDec :: GT.Dec -> Put
putDec (GT.Dec x) = putDoublele x
{-# INLINE putDec #-}

putPhotometry :: GT.Photometry -> Put
putPhotometry p@(GT.Photometry fg fbp frp) =
  let
    photometryByte = encodePhotometryStatus . photometryStatus $ p
  in
       putWord8 photometryByte
    >> putMeanFluxG fg
    >> putMeanFluxBP fbp
    >> putMeanFluxRP frp
{-# INLINE putPhotometry #-}

putMeanFlux :: GT.MeanFlux -> Put
putMeanFlux (GT.MeanFlux x) = putDoublele x
{-# INLINE putMeanFlux #-}

putMeanFluxG :: GT.MeanFluxG -> Put
putMeanFluxG (GT.MeanFluxG x) = putMeanFlux x
{-# INLINE putMeanFluxG #-}

putMeanFluxBP :: GT.MeanFluxBP -> Put
putMeanFluxBP mx
  = case mx of
      GT.NoBPBand     -> putDoublele 0
      GT.MeanFluxBP x -> putMeanFlux x
{-# INLINE putMeanFluxBP #-}

putMeanFluxRP :: GT.MeanFluxRP -> Put
putMeanFluxRP mx
  = case mx of
      GT.NoRPBand     -> putDoublele 0
      GT.MeanFluxRP x -> putMeanFlux x
{-# INLINE putMeanFluxRP #-}

-- Photometry Status bits -----------------------------------------------------

data PhotometryStatus
  = PhotometryStatus
    { psHasMeanFluxBP :: Bool
    , psHasMeanFluxRP :: Bool
    }

photometryStatus :: GT.Photometry -> PhotometryStatus
photometryStatus p
  = PhotometryStatus
    { psHasMeanFluxBP = GT.hasBPBand p
    , psHasMeanFluxRP = GT.hasRPBand p
    }

encodePhotometryStatus :: PhotometryStatus -> Word8
encodePhotometryStatus status =
  let
    b0 = if psHasMeanFluxBP status then bit 0 else 0
    b1 = if psHasMeanFluxRP status then bit 1 else 0
  in
    b0 .|. b1

decodePhotometryStatus :: Word8 -> PhotometryStatus
decodePhotometryStatus x =
  let
    hasMeanFluxBP = testBit x 0
    hasMeanFluxRP = testBit x 1
  in
    PhotometryStatus
    { psHasMeanFluxBP = hasMeanFluxBP
    , psHasMeanFluxRP = hasMeanFluxRP
    }
