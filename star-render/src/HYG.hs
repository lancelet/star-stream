{-|
Module      : HYG
Description : Loads files from the HYG database.

The HYG database is available on GitHub here:
  https://github.com/astronexus/HYG-Database

The functions in this file parse fields from the file 'hygdata_v3.csv'.
-}
{-# LANGUAGE OverloadedStrings #-}
module HYG
  ( -- * Functions
    runParseStar
  ) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text (Parser, char, count, parseOnly,
                                       skipWhile, takeWhile)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Prelude              (Either (Left, Right), Fractional, Int,
                                       Maybe (Just, Nothing), pure, ($), (*),
                                       (*>), (.), (/), (/=), (<$>), (<*>), (<>))

import           Star                 (Star)
import qualified Star                 as Star


-- | Runs the star parser on a line of input.
runParseStar
  :: Fractional a
  => Parser a              -- ^ Parser for the star's numeric fields.
  -> Text                  -- ^ Line of input to parse.
  -> Either Text (Star a)  -- ^ Parse outcome.
runParseStar parseNum txt =
  case parseOnly (parseStar parseNum) txt of
    Left err   -> Left $ Text.pack err <> " : " <> txt
    Right star -> Right star


-- | Parse a line containing a star.
parseStar
  :: Fractional a
  => Parser a          -- ^ Parser for the star's numeric fields.
  -> Parser (Star a)   -- ^ Parser for the star.
parseStar parseNum
  = Star.Star
    <$> (Star.RightAscension . hoursToDegrees <$> (dropFields 7 *> parseNum))
    <*> (Star.Declination                     <$> (dropFields 1 *> parseNum))
    <*> (Star.Magnitude                       <$> (dropFields 5 *> parseNum))
    <*> (Star.SpectralType                    <$> (dropFields 2 *> takeWhile ((/=) ',')))
    <*> (dropFields 1 *> (Just . Star.ColorIndex <$> parseNum
                          <|> pure Nothing))


-- | Converts a value expressed in hours (eg. RightAscension) to degrees.
hoursToDegrees :: Fractional a => a -> a
hoursToDegrees x = 360 * x / 24


-- | Drops 'nFields' comma-delimited fields.
dropFields
  :: Int         -- ^ Number of fields to drop.
  -> Parser ()   -- ^ Parser.
dropFields nFields = count nFields skipField *> pure ()
  where
    skipField = skipWhile ((/=) ',') *> char ',' *> pure ()
