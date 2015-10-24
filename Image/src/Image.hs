{-# LANGUAGE RankNTypes, ViewPatterns #-}

module Image (
    Image(..),
    index,
    generate,
    replicate,
    generateDeep,
    replicateDeep,
    toREPA,
    Drawer,
    drawings,
    draw,
    rect,
    line,
    blit,
    slice,
    load,
    savePng,
    (!),
    (!!),
    fromJuicyRGBA8,
    toJuicyRGBA8,
    encodePng,
    decodePng,
    encodePngBase64,
    encodePngBase64',
    printImage,
    printImageTop,
    module Pixel)
    where

import Control.Monad (when)
import Control.Monad.ST
import Data.Either (either)
import Data.Word
import Debug.Trace (traceShow)
import Linear
import Pixel
import Prelude hiding (replicate,(!!))
import Range (for)
import qualified Codec.Picture as J
import qualified Codec.Picture.Png as J
import qualified Codec.Picture.RGBA8 as J
import qualified Codec.Picture.Types as J
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.BMP as R
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Base64.Lazy as LBase64
import qualified Data.ByteString.Base64.URL.Lazy as LBase64Url
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Terminal

data Image = Image {
    imageSize        :: V2 Int,
    imageDepthBuffer :: UV.Vector Int,
    imagePixelBuffer :: UV.Vector Pixel} 
    deriving Show

{-# INLINE index #-}
index :: V2 Int -> V2 Int -> Int
index (V2 w h) (V2 x y) = y*w+x

{-# INLINE position #-}
position :: V2 Int -> Int -> V2 Int
position (V2 w h) index = V2 (mod index w) (div index w)

{-# INLINE replicate #-}
replicate :: V2 Int -> Int -> Pixel -> Image
replicate size@(V2 w h) depth pixel = Image size 
    (UV.replicate (w*h) depth) 
    (UV.replicate (w*h) pixel)

{-# INLINE replicateDeep #-}
replicateDeep :: V2 Int -> Pixel -> Image
replicateDeep size = replicate size (minBound::Int)

{-# INLINE generate #-}
generate :: V2 Int -> (V2 Int -> Int) -> (V2 Int -> Pixel) -> Image
generate size@(V2 w h) depth col = Image size 
    (UV.generate (w*h) (depth.position size)) 
    (UV.generate (w*h) (col.position size))

{-# INLINE generateDeep #-}
generateDeep :: V2 Int -> (V2 Int -> Pixel) -> Image
generateDeep size = generate size (const (minBound::Int))

{-# INLINE toREPA #-}
toREPA :: Image -> R.Array R.U R.DIM2 Pixel
toREPA (Image (V2 w h) _ pixelsBuffer) = R.fromUnboxed (R.Z R.:. h R.:. w) pixelsBuffer

{-# INLINE (!) #-}
(!) :: Image -> V2 Int -> Pixel
(!) (Image size _ pixelBuffer) position = pixelBuffer UV.! index size position

{-# INLINE (!!) #-}
(!!) :: Image -> V2 Int -> Int
(!!) (Image size depthBuffer _) position = depthBuffer UV.! index size position

slice :: V2 Int -> V2 Int -> Image -> Image
slice from to image = generate size (\pos -> image !! (pos ^+^ from)) (\pos -> image ! (pos ^+^ from))
    where size@(V2 w h) = to ^-^ from

type Writer    = forall s . (V2 Int -> Int -> Pixel -> ST s ()) -> ST s ()
newtype Drawer = Drawer { applyDrawer :: Writer }

{-# INLINE draw #-}
draw :: Drawer -> Image -> Image
draw drawer image@(Image size depthBuffer pixelBuffer) 
    = uncurry (Image size) $ runST $ do
        pixelMVec <- UV.thaw pixelBuffer
        depthMVec <- UV.thaw depthBuffer
        let V2 w h = size
        let writer pos@(V2 x y) depth pixel = do
                let idx      = index size pos
                when (x>=0 && y>=0 && x<w && y<h) $ do
                    current <- MUV.read depthMVec idx
                    when (depth >= current) $ do
                        MUV.unsafeWrite pixelMVec idx pixel
                        MUV.unsafeWrite depthMVec idx depth
        let (Drawer f) = drawer in f writer
        pixelVec <- UV.unsafeFreeze pixelMVec
        depthVec <- UV.unsafeFreeze depthMVec
        return $ (depthVec, pixelVec)
        
{-# INLINE drawings #-}
drawings :: Foldable t => t Drawer -> Drawer
drawings ds = Drawer (foldr cons nil ds) where
    cons (Drawer a) b fn = a fn >> b fn
    nil fn               = return ()

{-# INLINE rect #-}
rect :: Pixel -> V2 Int -> V2 Int -> Int -> Drawer
rect col (V2 ax ay) (V2 bx by) depth 
    = Drawer $ \ write ->
        for ax bx $ \x ->
            for ay by $ \y ->
                write (V2 x y) depth col

{-# INLINE line #-}
line :: V2 Int -> V2 Int -> Drawer
line (V2 x0 ya) (V2 x1 yb) = Drawer drawer where
    (xa, xb)               = (min x0 x1, max x0 x1)
    drawer :: Writer
    drawer set             = for xa xb drawPoint where
        drawPoint x        = set (V2 x y) 0 (rgba 0 0 0 0) where
            y              = floor $ f ya + f(x-xa) / f(xb-xa) * f(yb-ya)
            f              = fromIntegral

{-# INLINE blit #-}
blit :: V2 Int -> Image -> Drawer
blit from image  = Drawer drawer where
    drawer :: Writer
    drawer write = for (V2 0 0) (imageSize image) $ \ pos ->
        write (pos^+^from) (image !! pos) (image ! pos)

{-# INLINE fromJuicyRGBA8 #-}
fromJuicyRGBA8 :: J.Image J.PixelRGBA8 -> Image
fromJuicyRGBA8 jImage@(J.Image w h _) = generateDeep (V2 w h) getPixel where
    getPixel (V2 x y)                   = convertPixel $ J.pixelAt jImage x (h-y-1)
    convertPixel (J.PixelRGBA8 r g b a) = rgba r g b a

{-# INLINE toJuicyRGBA8 #-}
toJuicyRGBA8 :: Image -> J.Image J.PixelRGBA8
toJuicyRGBA8 image@(Image (V2 w h) _ pixelBuffer) = J.generateImage getPixel w h where
    getPixel x y                        = convertPixel $ image ! V2 x (h-y-1)
    convertPixel (getRGBA -> (r,g,b,a)) = J.PixelRGBA8 r g b a

{-# INLINE load #-}
load :: String -> IO Image
load = fmap fromJuicyRGBA8 . J.readImageRGBA8

{-# INLINE savePng #-}
savePng :: String -> Image -> IO ()
savePng path = J.savePngImage path . J.ImageRGBA8 . toJuicyRGBA8

{-# INLINE encodePng #-}
encodePng :: Image -> LB.ByteString
encodePng = J.encodePng . toJuicyRGBA8 

{-# INLINE decodePng #-}
decodePng :: B.ByteString -> Image
decodePng bytes = either error getImage (J.decodePng bytes) where
    getImage (J.ImageRGBA8 image) = fromJuicyRGBA8 image
    getImage _                    = error "Image.hs error: wrong image format."

{-# INLINE encodeBmp #-}
encodeBmp :: Image -> LB.ByteString
encodeBmp = J.encodeBitmap . toJuicyRGBA8 

{-# INLINE encodePngBase64 #-}
encodePngBase64 :: Image -> LB.ByteString
encodePngBase64 = LBase64.encode . encodePng

{-# INLINE encodePngBase64' #-}
encodePngBase64' :: Image -> B.ByteString
encodePngBase64' = LB.toStrict . encodePngBase64

{-# INLINE printImage #-}
printImage :: Image -> IO ()
printImage = putBase64ImageLn . encodePngBase64'

{-# INLINE printImageTop #-}
printImageTop :: Image -> IO ()
printImageTop = putBase64ImageTopLn . encodePngBase64'
