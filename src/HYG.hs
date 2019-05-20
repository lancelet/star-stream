{-|
Module      : HYG
Description : Loads files from the HYG database.

The HYG database is available on GitHub here:
  https://github.com/astronexus/HYG-Database

The functions in this file parse fields from the file 'hygdata_v3.csv'.
-}
module HYG
  ( -- * Functions
    runParseStar
  ) where

import           Data.Attoparsec.Text (Parser, char, count, parseOnly,
                                       skipWhile, takeWhile)
import           Data.Text            (Text)
import           Prelude              (Either, Int, String, pure, (*>), (/=),
                                       (<$>), (<*>))

import           Star                 (Star)
import qualified Star                 as Star


-- | Runs the star parser on a line of input.
runParseStar
  :: Parser a                -- ^ Parser for the star's numeric fields.
  -> Text                    -- ^ Line of input to parse.
  -> Either String (Star a)  -- ^ Parse outcome.
runParseStar parseNum = parseOnly (parseStar parseNum)


-- | Parse a line containing a star.
parseStar
  :: Parser a          -- ^ Parser for the star's numeric fields.
  -> Parser (Star a)   -- ^ Parser for the star.
parseStar parseNum
  = Star.Star
    <$> (Star.RightAscension <$> (dropFields 7 *> parseNum))
    <*> (Star.Declination    <$> (dropFields 1 *> parseNum))
    <*> (Star.Magnitude      <$> (dropFields 5 *> parseNum))
    <*> (Star.SpectralType   <$> (dropFields 2 *> takeWhile ((/=) ',')))
    <*> (Star.ColorIndex     <$> (dropFields 1 *> parseNum))


-- | Drops 'nFields' comma-delimited fields.
dropFields
  :: Int         -- ^ Number of fields to drop.
  -> Parser ()   -- ^ Parser.
dropFields nFields = count nFields skipField *> pure ()
  where
    skipField = skipWhile ((/=) ',') *> char ',' *> pure ()
