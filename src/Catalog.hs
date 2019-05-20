{-|
Module      : Catalog
Description : Processes star catalogs into acceleration structures.
-}
module Catalog where

import           Control.Monad        (forM_)
import qualified Data.Attoparsec.Text as Atto
import           Data.Either          (partitionEithers)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.IO         as TextIO

import qualified HYG                  as HYG
import           Star                 (Star)
import qualified Star                 as Star

parseCatalogLines
  :: (Text -> Either Text (Star a))
  -> Text
  -> ([Text], [Star a])
parseCatalogLines parseStar catalogText
  = partitionEithers $ parseStar <$> Text.lines catalogText


test :: IO [Star Double]
test = do
  txt' <- TextIO.readFile "data/HYG-Database/hygdata_v3.csv"
  let
    txt = Text.tail . Text.dropWhile ((/=) '\n') $ txt'
    (errs, stars) = parseCatalogLines (HYG.runParseStar Atto.double) txt
  forM_ errs TextIO.putStrLn
  pure stars


testBright :: IO [Star Double]
testBright =
  filter (\star -> Star.starMag star <= Star.Magnitude 6) <$> test
