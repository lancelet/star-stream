{-|
Module      : Star
Description : Data types defining a star.
-}
module Star where

import           Data.Text (Text)

newtype RightAscension a = RightAscension a deriving (Show, Eq, Ord)
newtype Declination a = Declination a deriving (Show, Eq, Ord)
newtype Magnitude a = Magnitude a deriving (Show, Eq, Ord)
newtype SpectralType = SpectralType Text deriving Show
newtype ColorIndex a = ColorIndex a deriving Show

data Star a
  = Star
    { starRA    :: !(RightAscension a)
    , starDec   :: !(Declination a)
    , starMag   :: !(Magnitude a)
    , starSpect :: !SpectralType
    , starCI    :: !(Maybe (ColorIndex a))
    } deriving Show
