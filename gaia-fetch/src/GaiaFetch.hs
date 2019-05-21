{-|
Module      : GaiaFetch
Description : Downloads ESA Gaia mission GDR2 data.

Gaia is a space observatory launched by the European Space Agency (ESA) in 2013:
  - https://en.wikipedia.org/wiki/Gaia_(spacecraft)
  - https://gea.esac.esa.int/archive/

The second data release from the mission, called GDR2, occurred on 25 April
2018. It consists of a set of GZipped CSV files, stored on an ESA server,
comprising a total of ~588 GB of compressed data.

This module contains functionality to download these files and check their
hashes against an MD5 manifest.

A description of the fields in these CSV files is found here:
  - https://gea.esac.esa.int/archive/documentation/GDR2/Gaia_archive/chap_datamodel/sec_dm_main_tables/ssec_dm_gaia_source.html
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GaiaFetch
  ( -- * Types
    GaiaLocalDir (GaiaLocalDir)
  , Env
    -- * Functions
  , defaultEnv
  , download'
  ) where

import           Conduit                             ((.|))
import qualified Conduit                             as Conduit
import           Control.Applicative                 (many, (<|>))
import           Control.Concurrent.MVar             (MVar)
import qualified Control.Concurrent.MVar             as MVar
import qualified Control.Concurrent.ParallelIO.Local as PIO
import           Control.Exception.Safe              (Exception,
                                                      Handler (Handler),
                                                      MonadMask, MonadThrow,
                                                      throw)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, ask,
                                                      runReaderT)
import           Control.Retry                       (RetryPolicy)
import qualified Control.Retry                       as Retry
import           Crypto.Hash                         (Digest, MD5)
import qualified Crypto.Hash                         as CryptoHash
import qualified Crypto.Hash.Conduit                 as CryptoHashConduit
import           Data.Attoparsec.Text                (Parser)
import qualified Data.Attoparsec.Text                as Atto
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Base16              as B16
import           Data.Char                           (isSpace)
import           Data.Maybe                          (catMaybes)
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as Text
import qualified Data.Text.IO                        as Text
import           Network.HTTP.Req                    (HttpConfig, Scheme (Http),
                                                      Url, (/:))
import qualified Network.HTTP.Req                    as Req
import qualified Network.HTTP.Req.Conduit            as Req (responseBodySource)
import qualified System.Directory                    as Directory

-- Types ----------------------------------------------------------------------

-- | Local directory to receieve the Gaia files.
newtype GaiaLocalDir = GaiaLocalDir FilePath deriving stock Show

-- | Name of a Gaia CSV file (including extension).
newtype CSVName = CSVName Text deriving stock Show

-- | Hash of a Gaia CSV file.
newtype CSVHash = CSVHash (Digest MD5) deriving stock Show

-- | CSV file and its associated hash.
data CSV = CSV CSVName CSVHash deriving stock Show

-- | Running environment.
data Env urlScheme
  = Env
    { -- | Optional maximum delay of retries (in seconds).
      cfgMaxDelayMicroseconds   :: Maybe Int
      -- | Base URL for the Gaia catalog.
    , cfgGaiaUrl                :: Url urlScheme
      -- | Maximum number of simultaneous downloads.
    , cfgNSimultaneousDownloads :: Int
      -- | Log function.
    , cfgLog                    :: Level -> Text -> IO ()
    }

-- | Log level.
data Level = Verbose | Info | Warning | Error

-- | Exception thrown if the MD5SUM.txt file could not be parsed.
data ErrorCouldNotParseMD5SUMFile = ErrorCouldNotParseMD5SUMFile Text
                                  deriving stock Show
instance Exception ErrorCouldNotParseMD5SUMFile

-- | Default environment.
defaultEnv :: IO (Env 'Http)
defaultEnv = do
  logLock <- MVar.newMVar ()
  pure Env
    { cfgMaxDelayMicroseconds   = Nothing
    , cfgGaiaUrl                = Req.http "cdn.gea.esac.esa.int" /: "Gaia"
    , cfgNSimultaneousDownloads = 1
    , cfgLog                    = defaultLogWithColors logLock
    }


-- URLs -----------------------------------------------------------------------

-- | URL of the 'MD5SUM.txt' file.
urlMd5Sum :: (MonadReader (Env s) m) => m (Url s)
urlMd5Sum = do
  base <- cfgGaiaUrl <$> ask
  pure $ base /: "gdr2" /: "gaia_source" /: "csv" /: "MD5SUM.txt"


-- | URL of a CSV file.
urlCSV :: (MonadReader (Env s) m) => CSV -> m (Url s)
urlCSV (CSV (CSVName name) _) = do
  base <- cfgGaiaUrl <$> ask
  pure $ base /: "gdr2" /: "gaia_source" /: "csv" /: name


-- Download -------------------------------------------------------------------

-- | Download action (specialized to IO).
download'
  :: Env s
  -> GaiaLocalDir
  -> IO ()
download' env localDir = runReaderT (download localDir) env


-- | Download action (generic).
download
  :: (MonadReader (Env s) m, MonadIO m, MonadMask m)
  => GaiaLocalDir
  -> m ()
download dir = do
  logInfo "== Gaia Fetch =="
  logVerbose "This utility downloads files from the Gaia GDR2 data release."
  logVerbose "Be warned that this is a large download (~588 GB), and may take"
  logVerbose "around 2 weeks to complete. This utility can be cancelled at any"
  logVerbose "time and will resume safely."

  csvs <- gaiaGetCSVs
  env <- ask
  liftIO $ PIO.withPool (cfgNSimultaneousDownloads env) $ \pool -> do
    let action csv = runReaderT (fetchSingle dir csv) env
    PIO.parallel_ pool (action <$> csvs)

  logInfo "== Download Complete =="


-- Gaia fetch operations ------------------------------------------------------

-- | Fetch all CSVs (names and MD5 hashes) from the Gaia archive.
gaiaGetCSVs
  :: (MonadReader (Env s) m, MonadIO m, MonadThrow m, MonadMask m)
  => m [CSV]
gaiaGetCSVs = do
  url <- urlMd5Sum
  logInfo $ "Fetching Gaia MD5SUM.txt file: " <> (Text.pack . show $ url)
  text <- httpGetText url
  logVerbose "Parsing Gaia MD5SUM.txt file"
  case Atto.parseOnly parseGaiaMD5SumFile text of
    Left err -> do
      logError $ "Could not parse Gaia MD5SUM.txt file: " <> (Text.pack err)
      throw . ErrorCouldNotParseMD5SUMFile . Text.pack $ err
    Right csvs -> do
      logVerbose "Successfully parsed Gaia MD5SUM.txt file"
      pure csvs


-- | Fetch a single file from the Gaia archive (with caching).
fetchSingle
  :: (MonadReader (Env s) m, MonadIO m, MonadThrow m, MonadMask m)
  => GaiaLocalDir
  -> CSV
  -> m ()
fetchSingle localDir csv = do
  let (CSV (CSVName fileName) (CSVHash expectedHash)) = csv
  logVerbose $ "Processing file: " <> fileName

  let
    GaiaLocalDir localFileDir = localDir
    fullLocalPath = localFileDir <> "/" <> (Text.unpack fileName)
  fileExists <- liftIO $ Directory.doesFileExist fullLocalPath

  if fileExists
  then do
    logVerbose $ "  Local file exists already; calculating MD5 hash: "
               <> (Text.pack fullLocalPath)
    fileHash <- md5HashFile fullLocalPath
    if fileHash /= expectedHash then do
      logWarning $ "  Local file hash "
                 <> (showt fileHash)
                 <> " did not match expected hash "
                 <> (showt expectedHash)
                 <> ". Re-downloading!"
      liftIO $ Directory.removeFile fullLocalPath
      fetchSingle localDir csv
    else do
      pure ()
  else do
    logInfo $ "Downloading file: "
            <> fileName
            <> " -> "
            <> (showt fullLocalPath)
    url <- urlCSV csv
    dlHash <- httpGetStreamAndHash fullLocalPath url
    if dlHash /= expectedHash then do
      logWarning $ "  Downloaded hash "
                 <> (showt dlHash)
                 <> " did not match expected hash "
                 <> (showt expectedHash)
                 <> ". Retrying!"
      liftIO $ Directory.removeFile fullLocalPath
      fetchSingle localDir csv
    else
      pure ()


-- Parsing --------------------------------------------------------------------

-- | Parse an entire Gaia MD5SUM file.
parseGaiaMD5SumFile :: Parser [CSV]
parseGaiaMD5SumFile = catMaybes <$> many lineParser
  where
    lineParser = (justCSV <|> nothingSkip) <* (many Atto.endOfLine)
    justCSV = parseCSV >>= pure . Just
    nothingSkip = skipExtraEntry >> pure Nothing


-- | Parses a Gaia CSV hash and file name.
--
-- Example:
--
-- >>> l = "9a157563ecfe89da0fb433c67b10ace1  GaiaSource_1_2.csv.gz"
-- >>> Atto.parseOnly parseCSV l
-- Right (CSV (CSVName "GaiaSource_1_2.csv.gz") (CSVHash 9a157563ecfe89da0fb433c67b10ace1))
parseCSV :: Parser CSV
parseCSV = do
  hash <- parseCSVHash <* Atto.skipSpace
  name <- parseCSVName <* Atto.skipSpace
  pure $ CSV name hash


-- | Skips an entry of MD5SUM.txt that is not a CSV file but still expected.
--
-- These files include:
--   - _citation.txt
--   - _disclaimer.txt
skipExtraEntry :: Parser ()
skipExtraEntry = do
  _ <- parseMD5Hash <* Atto.skipSpace
  _ <- (Atto.string "_citation.txt"
        <|> Atto.string "_disclaimer.txt"
       ) <* Atto.skipSpace
  pure ()


-- | Parses a Gaia CSV file name.
--
-- Gaia names must begin with "GaiaSource_",
--
-- Example:
--
-- >>> Atto.parseOnly parseCSVName "GaiaSource_1_2.csv.gz"
-- Right (CSVName "GaiaSource_1_2.csv.gz")
parseCSVName :: Parser CSVName
parseCSVName = do
  prefix    <- Atto.string "GaiaSource_"
  remainder <- Atto.takeWhile1 (not . isSpace)
  pure . CSVName $ prefix <> remainder


-- | Parses a Gaia CSV file MD5 hash.
parseCSVHash :: Parser CSVHash
parseCSVHash = CSVHash <$> parseMD5Hash


-- | Parses an MD5 hash.
--
-- Example:
--
-- >>> Atto.parseOnly parseMD5Hash "9a157563ecfe89da0fb433c67b10ace1"
-- Right 9a157563ecfe89da0fb433c67b10ace1
parseMD5Hash :: Parser (Digest MD5)
parseMD5Hash = do
  digits <- Atto.take 32
  maybeParser ("Could not parse \"" <> digits <> "\" as an MD5 digest.")
              (textToMD5Digest digits)


-- | Converts a 'Maybe' into a 'Parser', using a given message for failure.
maybeParser
  :: Text      -- ^ Message to use for failure.
  -> Maybe a   -- ^ Maybe result.
  -> Parser a  -- ^ Parser.
maybeParser failMessage maybeResult =
  case maybeResult of
    Just r  -> pure r
    Nothing -> fail . Text.unpack $ failMessage


-- | Converts a 'Text' value to an MD5 Digest.
--
-- Example:
--
-- >>> textToMD5Digest "9a157563ecfe89da0fb433c67b10ace1"
-- Just 9a157563ecfe89da0fb433c67b10ace1
--
-- >>> textToMD5Digest "fa"
-- Nothing
textToMD5Digest :: Text -> Maybe (Digest MD5)
textToMD5Digest text =
  case B16.decode (Text.encodeUtf8 text) of
    (bs, r) | BS.null r -> CryptoHash.digestFromByteString bs
    _                   -> Nothing


-- HTTP operations ------------------------------------------------------------

-- | Retry policy.
--
-- Retry using a constant delay of 5s. Keep retrying either forever, or until
-- the limit specified by the config is reached.
retryPolicy :: Env s -> RetryPolicy
retryPolicy env =
  let basePolicy = Retry.constantDelay 5000000
  in case cfgMaxDelayMicroseconds env of
    Nothing -> basePolicy
    Just n  -> Retry.capDelay n basePolicy


-- | HTTP configuration for Req.
httpConfig :: Env s -> HttpConfig
httpConfig env
  = Req.defaultHttpConfig
    { Req.httpConfigRetryPolicy = retryPolicy env }


-- | Retries when an HTTP exception occurs.
retryOnHttpException
  :: forall m s a.
     (MonadReader (Env s) m, MonadIO m, MonadMask m)
  => m a
  -> m a
retryOnHttpException action = do
  let
    shouldRetry :: Retry.RetryStatus -> Handler m Bool
    shouldRetry _ = Handler handle

    handle :: Req.HttpException -> m Bool
    handle _ = do
      logWarning "  Re-trying due to HttpException..."
      pure True

  rPolicy <- retryPolicy <$> ask
  Retry.recovering rPolicy [shouldRetry] (const action)


-- | HTTP GET.
--
-- This fetches an entire file at once.
httpGet
  :: (MonadReader (Env s) m, MonadIO m, MonadMask m)
  => Url s
  -> m ByteString
httpGet url = do
  httpCfg <- httpConfig <$> ask

  retryOnHttpException
    $ Req.runReq httpCfg
    $ Req.responseBody
    <$> Req.req Req.GET url Req.NoReqBody Req.bsResponse mempty


-- | HTTP GET returning a Text.
--
-- This fetches an entire file at once.
httpGetText
  :: (MonadReader (Env s) m, MonadIO m, MonadMask m)
  => Url s
  -> m Text
httpGetText url = Text.decodeUtf8 <$> httpGet url


-- | HTTP GET that streams to a file and computes the MD5 hash.
httpGetStreamAndHash
  :: (MonadReader (Env s) m, MonadIO m, MonadMask m)
  => FilePath
  -> Url s
  -> m (Digest MD5)
httpGetStreamAndHash filePath url = do
  httpCfg <- httpConfig <$> ask

  -- mess of Req and Conduit
  retryOnHttpException $ Req.runReq httpCfg $ do
    Req.reqBr Req.GET url Req.NoReqBody mempty $ \r ->
      Conduit.runConduitRes $
        Req.responseBodySource r .| (Conduit.getZipSink $
          Conduit.ZipSink (Conduit.sinkFile filePath) *>
          Conduit.ZipSink (CryptoHashConduit.sinkHash))


-- Local file operations ------------------------------------------------------

-- | Compute the MD5 hash of a local file, via Conduit streaming.
md5HashFile
  :: (MonadReader (Env s) m, MonadIO m, MonadMask m)
  => FilePath        -- ^ Path of the file.
  -> m (Digest MD5)  -- ^ Computed hash.
md5HashFile sourceFile =
  liftIO $ Conduit.withSourceFile sourceFile $ \source ->
    Conduit.runConduit $ source .| CryptoHashConduit.sinkHash

-- Log operations -------------------------------------------------------------

-- | Log a message.
logMsg :: (MonadReader (Env s) m, MonadIO m) => Level -> Text -> m ()
logMsg level msg = do
  logAction <- cfgLog <$> ask
  liftIO $ logAction level msg


-- | Log a message at 'Verbose' level.
logVerbose :: (MonadReader (Env s) m, MonadIO m) => Text -> m ()
logVerbose = logMsg Verbose


-- | Log a message at 'Info' level.
logInfo :: (MonadReader (Env s) m, MonadIO m) => Text -> m ()
logInfo = logMsg Info


-- | Log a message at 'Warning' level.
logWarning :: (MonadReader (Env s) m, MonadIO m) => Text -> m ()
logWarning = logMsg Warning


-- | Log a message at 'Error' level.
logError :: (MonadReader (Env s) m, MonadIO m) => Text -> m ()
logError = logMsg Error


-- | Default logging function with ANSI colors.
defaultLogWithColors :: MVar () -> Level -> Text -> IO ()
defaultLogWithColors lock level =
  defaultLogNoColors lock level . colorForLevel level


-- | Default logging function without colors.
defaultLogNoColors :: MVar () -> Level -> Text -> IO ()
defaultLogNoColors lock _ text = do
  MVar.takeMVar lock
  Text.putStrLn text
  MVar.putMVar lock ()


-- | Apply ANSI colors to a log level.
colorForLevel :: Level -> Text -> Text
colorForLevel level text = intro <> text <> reset
  where
    intro =
      case level of
        Verbose -> "\ESC[34m"  -- blue
        Info    -> "\ESC[0m"   -- reset (default)
        Warning -> "\ESC[33m"  -- yellow
        Error   -> "\ESC[31m"  -- red
    reset = "\ESC[0m"


-- | Show function for 'Text'.
showt :: Show a => a -> Text
showt = Text.pack . show


-- DocTest stuff --------------------------------------------------------------

{- $setup

>>> :set -XOverloadedStrings

-}
