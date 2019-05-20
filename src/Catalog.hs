{-|
Module      : Catalog
Description : Processes star catalogs into acceleration structures.
-}
module Catalog where

import qualified Data.Attoparsec.Text as Atto
import           Data.Either          (rights)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.IO         as TextIO

import qualified HYG                  as HYG
import           Star                 (Star)
import qualified Star                 as Star

parseCatalogLines
  :: (Text -> Either String (Star a))
  -> Text
  -> [Star a]
parseCatalogLines parseStar catalogText
  = rights $ parseStar <$> Text.lines catalogText


test :: IO [Star Double]
test = do
  txt' <- TextIO.readFile "data/HYG-Database/hygdata_v3.csv"
  let txt = Text.tail . Text.dropWhile ((/=) '\n') $ txt'
  pure $ parseCatalogLines (HYG.runParseStar Atto.double) txt


testBright :: IO [Star Double]
testBright =
  filter (\star -> Star.starMag star <= Star.Magnitude 4) <$> test
