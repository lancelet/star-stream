{-|
Module      : FluxWholeSky
Description : Whole-sky integrated flux image.

This module is a kind of prototype for integrated flux image
generation. It is intended for creating images of integrated flux over
the whole sky, so that we don't need to use any acceleration structure
for the star distribution.  Instead, all the stars are streamed
through sequentially. For each star, we compute its contribution to
the final flux image and mutate the image, adding in the star's flux.

The contribution of each star to the final image is computed using the
star location and the size of the reconstruction filter. The pixels
that the star can touch are computed first, and for each pixel, we
convolve the star flux in the three band-pass channels with the filter
function, to compute the contribution to that pixel.

More sophisticated rendering will probably use an acceleration /
bucketing structure to store the stars, so that smaller regions of the
sky can be rendered efficiently without streaming over all the stars
in the entire dataset.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module FluxWholeSky where

import qualified Codec.Picture                       as Juicy
import qualified Control.Concurrent.MVar             as MVar
import qualified Control.Concurrent.ParallelIO.Local as PIO
import           Control.Monad                       (forM_)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Primitive             (PrimMonad, PrimState)
import           Data.List                           (isPrefixOf, sort)
import           Data.List.Split                     (chunksOf)
import qualified Data.Vector.Unboxed                 as U
import           Data.Vector.Unboxed.Deriving        (derivingUnbox)
import qualified GHC.Compact                         as Compact
import qualified System.Directory                    as Directory

import qualified Gaia.CSVParser                      as CSVParser
import           Gaia.Types                          (ObservedSource)
import qualified Gaia.Types                          as GaiaTypes
import           Image                               (Image, MImage, PixelX,
                                                      PixelY)
import qualified Image                               as Image


data Flux
  = Flux
    { fluxG  :: {-# UNPACK #-} !Double
    , fluxBP :: {-# UNPACK #-} !Double
    , fluxRP :: {-# UNPACK #-} !Double
    }
  deriving Show


addFlux :: Flux -> Flux -> Flux
addFlux a b
  = Flux
    { fluxG  = fluxG a + fluxG b
    , fluxBP = fluxBP a + fluxBP b
    , fluxRP = fluxRP a + fluxRP b
    }
{-# INLINE addFlux #-}


scaleFlux :: Double -> Flux -> Flux
scaleFlux c x
  = Flux
    { fluxG  = c * fluxG x
    , fluxBP = c * fluxBP x
    , fluxRP = c * fluxRP x
    }
{-# INLINE scaleFlux #-}


derivingUnbox "Flux"
  [t| Flux -> (Double, Double, Double) |]
  [| \(Flux g bp rp) -> (g, bp, rp) |]
  [| \(g, bp, rp) -> Flux g bp rp |]


zeroFlux :: Flux
zeroFlux = Flux 0 0 0


fluxImageToJuicy :: Image Flux -> Juicy.Image Juicy.PixelRGBF
fluxImageToJuicy image =
  let
    fluxToFloatRGB8 flux =
      Image.FloatRGB
      { Image.frgbRed   = realToFrac . fluxRP $ flux
      , Image.frgbGreen = realToFrac . fluxG  $ flux
      , Image.frgbBlue  = realToFrac . fluxBP $ flux
      }
  in
    Image.toJuicy . Image.normalize . Image.mapImage fluxToFloatRGB8 $ image


pixelFilterRadius :: Double
pixelFilterRadius = sqrt 1.0


imageWidth :: Image.PixelWidth
imageWidth = Image.PixelWidth 5120


imageHeight :: Image.PixelHeight
imageHeight = Image.PixelHeight 2560


-- | Add the flux contributions of one star to the final image.
addStarFlux
  :: (PrimMonad m, s ~ PrimState m)
  => MImage s Flux    -- ^ Mutable image to operate on.
  -> ObservedSource   -- ^ Observed star.
  -> m ()             -- ^ Action.
addStarFlux image source = do
  forM_ (pixelsForStar source) $ \(px, py) -> do
    let flux = pixelFlux source (px, py)
    Image.modifyPixel image px py (addFlux flux)


-- | Mitchell filter from PBRT textbook.
--
-- Typical values:
--  b = 1/3, c = 1/3
mitchellFilter :: Double -> Double -> Double -> Double
mitchellFilter b c x' =
  let
    x = abs x'
  in
    if x > 1
    then ((-b - 6*c) * x*x*x + (6*b + 30*c) * x*x +
          (-12*b - 48*c) * x + (8*b + 24*c)) / 6
    else ((12 - 9*b - 6*c) * x*x*x +
          (-18 + 12*b + 6*c) * x*x +
          (6 - 2*b)) / 6


gaussianFilter :: Double -> Double
gaussianFilter x = exp(-2 * x)
{-# INLINE gaussianFilter #-}


filterFactor :: Double -> Double
filterFactor = mitchellFilter (1/3) (1/3)
{-# INLINE filterFactor #-}


stellarFlux :: ObservedSource -> Flux
stellarFlux source =
  let
    photometry = GaiaTypes.osPhotometry source
    GaiaTypes.MeanFluxG (GaiaTypes.MeanFlux g) = GaiaTypes.phoMeanFluxG photometry
    mfbp = GaiaTypes.phoMeanFluxBP photometry
    mfrp = GaiaTypes.phoMeanFluxRP photometry
  in
    case (mfbp, mfrp) of
      (GaiaTypes.MeanFluxBP (GaiaTypes.MeanFlux bp),
       GaiaTypes.MeanFluxRP (GaiaTypes.MeanFlux rp)) ->
        Flux
        { fluxG = g
        , fluxBP = bp
        , fluxRP = rp
        }
      _ ->
        Flux
        { fluxG  = g
        , fluxBP = g
        , fluxRP = g
        }
{-# INLINE stellarFlux #-}


pixelFlux :: ObservedSource -> (PixelX, PixelY) -> Flux
pixelFlux source (Image.PixelX px, Image.PixelY py) =
  let
    position = GaiaTypes.osPosition source
    GaiaTypes.RA ra = GaiaTypes.posRA position
    GaiaTypes.Dec dec = GaiaTypes.posDec position
    x = 0.5 + fromIntegral px
    y = 0.5 + fromIntegral py
    Image.PixelWidth w' = imageWidth
    Image.PixelHeight h' = imageHeight
    w = fromIntegral w'
    h = fromIntegral h'

    starX = ra * w / 360
    starY = (dec + 90) * h / 180

    dx = x - starX
    dy = y - starY

    filterDist = (sqrt (dx*dx + dy*dy)) / pixelFilterRadius
  in
    if filterDist > 1.0
      then zeroFlux
      else scaleFlux (filterFactor filterDist) (stellarFlux source)
{-# INLINE pixelFlux #-}


pixelsForStar :: ObservedSource -> [ (PixelX, PixelY) ]
pixelsForStar source =
  let
    pxrect = pixelRectForStar source
  in
    [ (px, py)
    | px <- [ prMinX pxrect .. prMaxX pxrect ]
    , py <- [ prMinY pxrect .. prMaxY pxrect ]
    ]
{-# INLINE pixelsForStar #-}


pixelRectForStar :: ObservedSource -> PixelRect
pixelRectForStar source =
  let
    position = GaiaTypes.osPosition source
    GaiaTypes.RA ra = GaiaTypes.posRA position
    GaiaTypes.Dec dec = GaiaTypes.posDec position
    Image.PixelWidth w' = imageWidth
    Image.PixelHeight h' = imageHeight
    w = fromIntegral w'
    h = fromIntegral h'
    xpx = ra * w / 360
    ypx = (dec + 90) * h / 180
    minx = floor $ xpx - pixelFilterRadius
    miny = floor $ ypx - pixelFilterRadius
    maxx = ceiling $ xpx + pixelFilterRadius
    maxy = ceiling $ ypx + pixelFilterRadius
    minx' = if minx < 0 then 0 else minx
    miny' = if miny < 0 then 0 else miny
    maxx' = if maxx > (w' - 1) then (w' - 1) else maxx
    maxy' = if maxy > (h' - 1) then (h' - 1) else maxy
  in
    PixelRect
    { prMinX = Image.PixelX minx'
    , prMaxX = Image.PixelX maxx'
    , prMinY = Image.PixelY miny'
    , prMaxY = Image.PixelY maxy'
    }
{-# INLINE pixelRectForStar #-}


data PixelRect
  = PixelRect
    { prMinX :: {-# UNPACK #-} !PixelX
    , prMaxX :: {-# UNPACK #-} !PixelX
    , prMinY :: {-# UNPACK #-} !PixelY
    , prMaxY :: {-# UNPACK #-} !PixelY
    }


-- | Compute whole-sky flux image by streaming over all the stars.
fluxWholeSky
  :: (MonadIO m, PrimMonad m)
  => FilePath        -- ^ Directory containing Gaia CSV files.
  -> m (Image Flux)  -- ^ Action returning the flux image.
fluxWholeSky gaiaCSVdir = do

  -- find the Gaia files
  dirContents <- liftIO $ Directory.listDirectory gaiaCSVdir
  let
    isGaiaFile filePath = "GaiaSource_" `isPrefixOf` filePath
    gaiaFiles = {- take 1000
              $ -} fmap (\f -> gaiaCSVdir <> "/" <> f)
              $ sort
              $ filter isGaiaFile dirContents

  -- create the base image
  image <- liftIO $ Image.newMImage imageWidth imageHeight zeroFlux

  -- locks
  logLock <- liftIO $ MVar.newMVar ()
  imgLock <- liftIO $ MVar.newMVar ()

  let
    withLogLock :: IO a -> IO a
    withLogLock action = do
      MVar.takeMVar logLock
      r <- action
      MVar.putMVar logLock ()
      pure r

    withImgLock :: IO a -> IO a
    withImgLock action = do
      MVar.takeMVar imgLock
      r <- action
      MVar.putMVar imgLock ()
      pure r

  -- for each file, add all the stellar flux
  let
    contributeFiles :: [FilePath] -> IO ()
    contributeFiles files = do

      cImage <- Image.newMImage imageWidth imageHeight zeroFlux

      let
        contributeFile file = do
          withLogLock $ putStrLn $ "Processing: " <> file

          obsList <- CSVParser.readCSVGZFile file
          let observations = U.fromList obsList
          _ <- Compact.compact observations
          U.forM_ observations $ \source -> addStarFlux cImage source

      forM_ files contributeFile
      withImgLock $ Image.binOpMImages addFlux image cImage

  -- spawn multiple threads to handle chunks of the input files
  _ <- liftIO $ PIO.withPool 10 $ \pool ->
    PIO.parallel_ pool (contributeFiles <$> (chunksOf 512 gaiaFiles))

  -- freeze and return the mutable image
  liftIO $ Image.freeze image


-- | Test drive the fluxWholeSky action.
--
-- Reads data from "/Volumes/Gaia/gaia"
-- Writes to "test.hdr"
testFluxWholeSky :: IO ()
testFluxWholeSky = do
  let inputDir = "/Volumes/Gaia/gaia"

  image <- fluxWholeSky inputDir

  let juicyImage = fluxImageToJuicy image
  Juicy.writeHDR "test.hdr" juicyImage
