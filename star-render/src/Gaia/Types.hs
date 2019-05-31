{-|
Module      : Gaia.Types
Description : Types for the Gaia module.

See https://gea.esac.esa.int/archive/documentation/GDR2/Gaia_archive/chap_datamodel/sec_dm_main_tables/ssec_dm_gaia_source.html
for more information.

Only the fields that we require are represented. The actual Gaia dataset
contains much more information.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Gaia.Types where

import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Data.Word                    (Word64)

-- | Gaia DPAC solution identifier.
newtype SolutionId = SolutionId Word64

derivingUnbox "SolutionId"
  [t| SolutionId -> Word64 |] [| \(SolutionId i) -> i |] [| SolutionId |]

-- | Gaia unique source identifier for DR2.
newtype SourceId = SourceId Word64

derivingUnbox "SourceId"
  [t| SourceId -> Word64 |] [| \(SourceId i) -> i |] [| SourceId |]

-- | Random index used to select subsets.
newtype RandomIndex = RandomIndex Word64

derivingUnbox "RandomIndex"
  [t| RandomIndex -> Word64 |] [| \(RandomIndex i) -> i |] [| RandomIndex |]

-- | Identification of an observed source.
data Id
  = Id
    { idSolutionId  :: {-# UNPACK #-} !SolutionId
    , idSourceId    :: {-# UNPACK #-} !SourceId
    , idRandomIndex :: {-# UNPACK #-} !RandomIndex
    }

derivingUnbox "Id"
  [t| Id -> (SolutionId, SourceId, RandomIndex) |]
  [| \i -> (idSolutionId i, idSourceId i, idRandomIndex i) |]
  [| \(solutionId, sourceId, randomIndex) ->
       Id solutionId sourceId randomIndex |]

-- | Right ascension (angle in degrees).
newtype RA = RA Double

derivingUnbox "RA"
  [t| RA -> Double |] [| \(RA x) -> x |] [| RA |]
  
-- | Declination (angle in degrees).
newtype Dec = Dec Double

derivingUnbox "Dec"
  [t| Dec -> Double |] [| \(Dec x) -> x |] [| Dec |]

-- | Position information for an observed source.
data Position
  = Position
    { posRA  :: {-# UNPACK #-} !RA
    , posDec :: {-# UNPACK #-} !Dec
    }

derivingUnbox "Position"
  [t| Position -> (RA, Dec) |]
  [| \p -> (posRA p, posDec p) |]
  [| \(ra, dec) -> Position ra dec |]

-- | Mean flux (units uncertain - further research required).
newtype MeanFlux = MeanFlux Double

derivingUnbox "MeanFlux"
  [t| MeanFlux -> Double |] [| \(MeanFlux x) -> x |] [| MeanFlux |]
  
-- | Mean flux in G band.
newtype MeanFluxG = MeanFluxG MeanFlux

derivingUnbox "MeanFluxG"
  [t| MeanFluxG -> MeanFlux |] [| \(MeanFluxG x) -> x |] [| MeanFluxG |]
  
-- | Mean flux in BP band.
data MeanFluxBP
  = NoBPBand
  | MeanFluxBP !MeanFlux

derivingUnbox "MeanFluxBP"
  [t| MeanFluxBP -> (Bool, MeanFlux) |]
  [| \m -> case m of
             NoBPBand     -> (False, MeanFlux 0)
             MeanFluxBP x -> (True, x) |]
  [| \(b, m) -> if b then MeanFluxBP m else NoBPBand |]
  
-- | Mean flux in RP band.
data MeanFluxRP
  = NoRPBand
  | MeanFluxRP !MeanFlux

derivingUnbox "MeanFluxRP"
  [t| MeanFluxRP -> (Bool, MeanFlux) |]
  [| \m -> case m of
             NoRPBand     -> (False, MeanFlux 0)
             MeanFluxRP x -> (True, x) |]
  [| \(b, m) -> if b then MeanFluxRP m else NoRPBand |]

-- | Photometry data from the star.
data Photometry
  = Photometry
    { phoMeanFluxG  :: {-# UNPACK #-} !MeanFluxG
    , phoMeanFluxBP :: !MeanFluxBP
    , phoMeanFluxRP :: !MeanFluxRP
    }

derivingUnbox "Photometry"
  [t| Photometry -> (MeanFluxG, MeanFluxBP, MeanFluxRP) |]
  [| \p -> (phoMeanFluxG p, phoMeanFluxBP p, phoMeanFluxRP p) |]
  [| \(meanFluxG, meanFluxBP, meanFluxRP) ->
       Photometry meanFluxG meanFluxBP meanFluxRP |]

hasBPBand :: Photometry -> Bool
hasBPBand photometry
  = case phoMeanFluxBP photometry of
      NoBPBand     -> False
      MeanFluxBP _ -> True

hasRPBand :: Photometry -> Bool
hasRPBand photometry
  = case phoMeanFluxRP photometry of
      NoRPBand     -> False
      MeanFluxRP _ -> True

-- | Definition of an observed source.
data ObservedSource
  = ObservedSource
    { osId         :: {-# UNPACK #-} !Id
    , osPosition   :: {-# UNPACK #-} !Position
    , osPhotometry :: {-# UNPACK #-} !Photometry
    }

derivingUnbox "ObservedSource"
  [t| ObservedSource -> (Id, Position, Photometry) |]
  [| \s -> (osId s, osPosition s, osPhotometry s) |]
  [| \(identifier, position, photometry) ->
       ObservedSource identifier position photometry |]
