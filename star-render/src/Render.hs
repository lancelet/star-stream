{-|
Module      : Render
Description : Basic starfield render.

This is all crazy-WIP.
-}
module Render where

import           Codec.Picture (DynamicImage (ImageRGB8), Pixel8,
                                PixelRGB8 (PixelRGB8), generateImage,
                                savePngImage)

import qualified Catalog
import           Star          (Star)
import qualified Star


gaussianFilter
  :: Double
  -> Double
  -> Double
  -> Double
gaussianFilter x y r =
  let
    x' = 2 * x / r
    y' = 2 * y / r
    r' = x'*x' + y'*y'
  in
    exp(-2 * r')


newtype ImgWidth = ImgWidth Int
newtype ImgHeight = ImgHeight Int

latLonImageFn
  :: [Star Double]
  -> ImgWidth
  -> ImgHeight
  -> Int
  -> Int
  -> PixelRGB8
latLonImageFn stars (ImgWidth w) (ImgHeight h) i j =
  let
    x, y :: Double
    x = 360 * fromIntegral i / fromIntegral w
    y = 180 * fromIntegral j / fromIntegral h - 90

    filterRadius :: Double
    filterRadius = 2.8

    radius :: Double
    radius = filterRadius * max (360 / fromIntegral w) (180 / fromIntegral h)

    starsInRadius :: [Star Double]
    starsInRadius
      = filter (\star ->
          let
            Star.RightAscension ra  = Star.starRA star
            Star.Declination    dec = Star.starDec star
            dx = x - ra
            dy = y - dec
            l  = dx*dx + dy*dy
          in
            l < (radius * radius)
        ) stars

    brightness :: Double
    brightness =
      let
        b' = sum $ map (\star ->
               let
                 Star.RightAscension ra  = Star.starRA star
                 Star.Declination    dec = Star.starDec star
                 Star.Magnitude      m   = Star.starMag star
                 m' = if m < 0.25 then 0.25 else m  -- futz
                 dx = x - ra
                 dy = y - dec
                 b  = 1 / m'  -- futz the brightness
               in
                 b * gaussianFilter dx dy radius
             ) starsInRadius
      in
        if b' > 1.0 then 1.0 else b'

    px :: Pixel8
    px = floor (brightness * 255)
  in
    PixelRGB8 px px px


latLonTest :: IO ()
latLonTest = do
  brightStars <- Catalog.testBright
  let
    w = 800
    h = 400
    f = latLonImageFn brightStars (ImgWidth w) (ImgHeight h)
    image = generateImage f w h
  savePngImage "test.png" (ImageRGB8 image)
