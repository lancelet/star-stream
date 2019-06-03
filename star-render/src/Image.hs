{-|
Module      : Image
Description : Linear-space images.
-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Image where

import qualified Codec.Picture                as Juicy
import           Control.DeepSeq              (NFData)
import           Control.Monad.Primitive      (PrimMonad, PrimState)
import           Data.Vector.Unboxed          (Unbox)
import qualified Data.Vector.Unboxed          as U
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Data.Vector.Unboxed.Mutable  as UM
import           Data.Word                    (Word16)
import           GHC.Generics                 (Generic)

-- | Pixel x coordinate.
newtype PixelX = PixelX Word16 deriving (Enum)

-- | Pixel y coordinate.
newtype PixelY = PixelY Word16 deriving (Enum)

-- | Width in pixels.
newtype PixelWidth = PixelWidth Word16
  deriving stock (Eq, Generic)
  deriving anyclass NFData

-- | Height in pixels.
newtype PixelHeight = PixelHeight Word16
  deriving stock (Eq, Generic)
  deriving anyclass NFData

-- | Immutable vector of pixel data.
newtype PixelData a = PixelData (U.Vector a)

-- | Mutable vector of pixel data.
newtype MPixelData s a = MPixelData (UM.MVector s a)
  deriving stock Generic
  deriving anyclass NFData

-- | Linear index into pixel data.
newtype LinearIndex = LinearIndex Int


-- | Floating-point RGB pixel.
data FloatRGB
  = FloatRGB
    { frgbRed   :: {-# UNPACK #-} !Float
    , frgbGreen :: {-# UNPACK #-} !Float
    , frgbBlue  :: {-# UNPACK #-} !Float
    }

derivingUnbox "FloatRGB"
  [t| FloatRGB -> (Float, Float, Float) |]
  [| \(FloatRGB r g b) -> (r, g, b) |]
  [| \(r, g, b) -> FloatRGB r g b |]


-- | Convert a floating-point RGB pixel to its equivalent in JuicyPixels.
floatRGBToPixelRGBF :: FloatRGB -> Juicy.PixelRGBF
floatRGBToPixelRGBF (FloatRGB r g b) = Juicy.PixelRGBF r g b


-- | Mutable image.
data MImage s a
  = MImage
    { miWidth  :: {-# UNPACK #-} !PixelWidth
    , miHeight :: {-# UNPACK #-} !PixelHeight
    , miData   :: !(MPixelData s a)
    }
  deriving (Generic, NFData)


-- | Image.
data Image a
  = Image
    { iWidth  :: {-# UNPACK #-} !PixelWidth
    , iHeight :: {-# UNPACK #-} !PixelHeight
    , iData   :: !(PixelData a)
    }



binOpMImages
  :: (PrimMonad m, Unbox a, s ~ PrimState m)
  => (a -> a -> a)
  -> MImage s a
  -> MImage s a
  -> m ()
binOpMImages op source extra =
  let
    sourceDim = (miWidth source, miHeight source)
    extraDim = (miWidth extra, miHeight extra)
    MPixelData sourcePx = miData source
    MPixelData extraPx = miData extra
  in
    if sourceDim /= extraDim
    then error $ "binOpMImages needs images the same size!"
    else do
      let len = UM.length sourcePx
      loop 0 (\i -> i < len) (\i -> i + 1) $ \i -> do
        s <- UM.unsafeRead sourcePx i
        e <- UM.unsafeRead extraPx i
        UM.unsafeWrite sourcePx i (op s e)


loop
  :: (Monad m)
  => a             -- ^ starting value of loop
  -> (a -> Bool)   -- ^ terminate when this function returns False
  -> (a -> a)      -- ^ state transition
  -> (a -> m ())   -- ^ loop body
  -> m ()
loop start while step body = go start
  where
    go !i | while i   = body i >> go (step i)
          | otherwise = return ()
{-# INLINE loop #-}


-- | Create a new mutable image.
newMImage
  :: (PrimMonad m, Unbox a, s ~ PrimState m)
  => PixelWidth      -- ^ Width of the image.
  -> PixelHeight     -- ^ Height of the image.
  -> a               -- ^ Value to fill the image with.
  -> m (MImage s a)  -- ^ Image produced.
newMImage pxw pxh value = do
  let
    PixelWidth w' = pxw
    PixelHeight h' = pxh
    w = fromIntegral w'
    h = fromIntegral h'
    n = w * h
  vec <- UM.replicate n value
  pure $ MImage
    { miWidth  = pxw
    , miHeight = pxh
    , miData   = MPixelData vec
    }
{-# INLINE newMImage #-}


-- | Return a linear index (row major) into a mutable image.
--
-- This converts a 2D pixel index into a linear index used for the underlying
-- vector that stores the image pixel data. No bounds-check is performed.
--
-- For a version with an additional bounds-check, see @lMIndex@.
lMIndexUnsafe
  :: MImage s a   -- ^ Image.
  -> PixelX       -- ^ Pixel x coordinate.
  -> PixelY       -- ^ Pixel y coordinate.
  -> LinearIndex  -- ^ Linear index into the image.
lMIndexUnsafe image (PixelX x') (PixelY y') =
  let
    PixelWidth w' = miWidth image
    x = fromIntegral x'
    y = fromIntegral y'
    w = fromIntegral w'
  in
    LinearIndex $ x + y * w
{-# INLINE lMIndexUnsafe #-}


-- | Return a linear index (row major) into an image.
--
-- This converts a 2D pixel index into a linear index used for the underlying
-- vector that stores the image pixel data. No bounds-check is performed.
--
-- For a version with an additional bounds-check, see @lIndex@.
lIndexUnsafe
  :: Image a      -- ^ Image.
  -> PixelX       -- ^ Pixel x coordinate.
  -> PixelY       -- ^ Pixel y coordinate.
  -> LinearIndex  -- ^ Linear index into the image.
lIndexUnsafe image (PixelX x') (PixelY y') =
  let
    PixelWidth w' = iWidth image
    x = fromIntegral x'
    y = fromIntegral y'
    w = fromIntegral w'
  in
    LinearIndex $ x + y * w
{-# INLINE lIndexUnsafe #-}


-- | Return a linear index (row major) into a mutable image.
--
-- This converts a 2D pixel index into a linear index used for the underlying
-- vector that stores the image pixel data. A bounds-check is performed.
--
-- For an unsafe version, see @lMIndexUnsafe@.
lMIndex
  :: MImage s a    -- ^ Image.
  -> PixelX        -- ^ Pixel x coordinate.
  -> PixelY        -- ^ Pixel y coordinate.
  -> LinearIndex   -- ^ Linear index into the image.
lMIndex image px py =
  let
    PixelX x = px
    PixelY y = py
    PixelWidth w = miWidth image
    PixelHeight h = miHeight image
  in
    if x < w && y < h
    then lMIndexUnsafe image px py
    else error $ "Index (" <> show x <> ", " <> show y <> ")"
                 <> "is out of bounds!"
{-# INLINE lMIndex #-}


-- | Return a linear index (row major) into an image.
--
-- This converts a 2D pixel index into a linear index used for the underlying
-- vector that stores the image pixel data. A bounds-check is performed.
--
-- For an unsafe version, see @lIndexUnsafe@.
lIndex
  :: Image a       -- ^ Image.
  -> PixelX        -- ^ Pixel x coordinate.
  -> PixelY        -- ^ Pixel y coordinate.
  -> LinearIndex   -- ^ Linear index into the image.
lIndex image px py =
  let
    PixelX x = px
    PixelY y = py
    PixelWidth w = iWidth image
    PixelHeight h = iHeight image
  in
    if x < w && y < h
    then lIndexUnsafe image px py
    else error $ "Index (" <> show x <> ", " <> show y <> ")"
                 <> "is out of bounds!"
{-# INLINE lIndex #-}


-- | Modify a single pixel in an immutable image.
modifyPixel
  :: (PrimMonad m, Unbox a, s ~ PrimState m)
  => MImage s a
  -> PixelX
  -> PixelY
  -> (a -> a)
  -> m ()
modifyPixel image px py op = do
  let
    LinearIndex i = lMIndexUnsafe image px py
    MPixelData vec = miData image
  UM.unsafeModify vec op i
{-# INLINE modifyPixel #-}


-- | Yield an immutable image from a mutable one.
freeze
  :: (PrimMonad m, s ~ PrimState m, Unbox a)
  => MImage s a   -- ^ Image to freeze.
  -> m (Image a)  -- ^ Frozen image.
freeze mImage = do
  let
    MPixelData mVec = miData mImage
  vec <- U.freeze mVec
  pure $ Image
    { iWidth  = miWidth mImage
    , iHeight = miHeight mImage
    , iData   = PixelData vec
    }
{-# INLINE freeze #-}


-- Get a pixel from an image.
getPixel
  :: (Unbox a)
  => Image a   -- ^ Image.
  -> PixelX    -- ^ X-coordinate of pixel to get.
  -> PixelY    -- ^ Y-coordinate of pixel to get.
  -> a         -- ^ Pixel value.
getPixel image px py =
  let
    LinearIndex i = lIndex image px py
    PixelData vec = iData image
  in
    vec U.! i
{-# INLINE getPixel #-}


-- | Map a function over all the values in an image.
mapImage
  :: (Unbox a, Unbox b)
  => (a -> b)  -- ^ Function to map.
  -> Image a   -- ^ Input image.
  -> Image b   -- ^ Output image.
mapImage f image =
  let
    PixelData vec = iData image
    vec' = U.map f vec
  in
    image { iData = PixelData vec' }
{-# INLINE mapImage #-}


normalize :: Image FloatRGB -> Image FloatRGB
normalize image =
  let
    PixelData vec = iData image
    maxChannel (FloatRGB r g b) = max r (max g b)
    m = U.maximum $ U.map maxChannel vec
    nf (FloatRGB r g b) = FloatRGB (r / m) (g / m) (b / m)
  in
    mapImage nf image


binOpImages
  :: (Unbox a)
  => (a -> a -> a)
  -> Image a
  -> Image a
  -> Image a
binOpImages binop ia ib =
  let
    aDim = (iWidth ia, iHeight ia)
    bDim = (iWidth ib, iHeight ib)
    PixelData apx = iData ia
    PixelData bpx = iData ib
  in
    if aDim /= bDim
    then error $ "Attempted to call binOpImages with images "
                 <> "that are different dimensions!"
    else
      Image
      { iWidth  = iWidth ia
      , iHeight = iHeight ia
      , iData   = PixelData $ U.zipWith binop apx bpx
      }



-- | Convert a floating-point RGB image to a JuicyPixels image.
toJuicy :: Image FloatRGB -> Juicy.Image Juicy.PixelRGBF
toJuicy image =
  let
    PixelWidth w' = iWidth image
    PixelHeight h' = iHeight image
    w = fromIntegral w'
    h = fromIntegral h'
    px x y =
      let
        pixX = PixelX . fromIntegral $ x
        pixY = PixelY . fromIntegral $ y
      in
        floatRGBToPixelRGBF $ getPixel image pixX pixY
  in
    Juicy.generateImage px w h
