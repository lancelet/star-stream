{-|
Module      : Gaia.Types
Description : Types for the Gaia module.

See https://gea.esac.esa.int/archive/documentation/GDR2/Gaia_archive/chap_datamodel/sec_dm_main_tables/ssec_dm_gaia_source.html
for more information.

Only the fields that we require are represented. The actual Gaia dataset
contains much more information.
-}
module Gaia.Types where

import           Data.Word (Word64)

-- | Gaia DPAC solution identifier.
newtype SolutionId = SolutionId Word64
-- | Gaia unique source identifier for DR2.
newtype SourceId = SourceId Word64
-- | Random index used to select subsets.
newtype RandomIndex = RandomIndex Word64

-- | Identification of an observed source.
data Id
  = Id
    { idSolutionId  :: {-# UNPACK #-} !SolutionId
    , idSourceId    :: {-# UNPACK #-} !SourceId
    , idRandomIndex :: {-# UNPACK #-} !RandomIndex
    }

-- | Right ascension (angle in degrees).
newtype RA = RA Double
-- | Declination (angle in degrees).
newtype Dec = Dec Double

-- | Position information for an observed source.
data Position
  = Position
    { posRA  :: {-# UNPACK #-} !RA
    , posDec :: {-# UNPACK #-} !Dec
    }

-- | Mean flux (units uncertain - further research required).
newtype MeanFlux = MeanFlux Double
-- | Mean flux in G band.
newtype MeanFluxG = MeanFluxG MeanFlux
-- | Mean flux in BP band.
newtype MeanFluxBP = MeanFluxBP MeanFlux
-- | Mean flux in RP band.
newtype MeanFluxRP = MeanFluxRP MeanFlux

-- | Photometry data from the star.
data Photometry
  = Photometry
    { phoMeanFluxG  :: {-# UNPACK #-} !MeanFluxG
    , phoMeanFluxBP :: {-# UNPACK #-} !(Maybe MeanFluxBP)
    , phoMeanFluxRP :: {-# UNPACK #-} !(Maybe MeanFluxRP)
    }

-- | Definition of an observed source.
data ObservedSource
  = ObservedSource
    { osId         :: {-# UNPACK #-} !Id
    , osPosition   :: {-# UNPACK #-} !Position
    , osPhotometry :: {-# UNPACK #-} !Photometry
    }
