{-# LANGUAGE ViewPatterns, TypeFamilies, TemplateHaskell, MultiParamTypeClasses, RankNTypes #-}

module Pixel where

import Data.Bits
import Data.Fixed (mod')
import Data.Word
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as U

newtype Pixel_ a = Pixel a deriving Show
type Pixel       = Pixel_ Word32
type HSL         = (Double,Double,Double)
type RGB         = (Word8,Word8,Word8)
type RGBA        = (Word8,Word8,Word8,Word8)
type RGBf        = (Double,Double,Double)

derivingUnbox "Pixel_"
    [t| forall a . (U.Unbox a) => Pixel_ a -> a |]
    [| \ (Pixel a) -> a |]
    [| \ a -> (Pixel a) |]

-- instance ListIsomorphic Pixel_ where
    -- type Elem Pixel_ a = a~Word32
    -- fromList [r,g,b,a] = rgba r g b a
    -- toList pixel       = [red pixel, green pixel, blue pixel, alpha pixel]

rgba :: (Integral a) => a -> a -> a -> a -> Pixel
rgba r g b a = let f = fromIntegral in Pixel (f r + (shiftL (f g) 8) + (shiftL (f b) 16) + (shiftL (f a) 24))

rgbaf :: (RealFrac f) => f -> f -> f -> f -> Pixel
rgbaf r g b a = rgba (floor (r*255)) (floor (g*255)) (floor (b*255)) (floor (a*255))

-- Converts an HSL color value to RGB. Conversion formula
-- adapted from http://en.wikipedia.org/wiki/HSL_color_space.
-- Assumes h, s, and l are contained in the set [0, 1] and
-- returns r, g, and b in the set [0, 255].
-- src: http://stackoverflow.com/a/9493060/1031791
hsl :: Double -> Double -> Double -> Pixel
hsl h s l | s == 0    = rgba (round (l*255)) (round (l*255)) (round (l*255)) 255
hsl h s l | otherwise = rgba (round (r*255)) (round (g*255)) (round (b*255)) 255 where
    hue2rgb p q ((`mod'` 1) -> t)
        | t < 1/6   = p + (q - p) * 6 * t
        | t < 3/6   = q
        | t < 4/6   = p + (q - p) * (4/6 - t) * 6
        | otherwise = p
    q = if l < 0.5 then l * (1 + s) else l + s - l * s
    p = 2 * l - q
    r = hue2rgb p q (h+1/3)
    g = hue2rgb p q h
    b = hue2rgb p q (h-1/3)

getRGB :: Pixel -> RGB
getRGB pixel = (red pixel, green pixel, blue pixel)

getRGBA :: Pixel -> RGBA
getRGBA pixel = (red pixel, green pixel, blue pixel, alpha pixel)

getRGBf :: Pixel -> RGBf
getRGBf pixel = (fromIntegral (red pixel) / 255, fromIntegral (green pixel) / 255, fromIntegral (blue pixel) / 255)

getHSL :: Pixel -> HSL
getHSL pixel = (h,s,l) where
    (r,g,b) = getRGBf pixel
    max = maximum [r,g,b]
    min = minimum [r,g,b]
    l = (max+min)/2
    d = (max-min)
    s = (if max == min then 0 else if l > 0.5 then d / (2-max-min) else d/(max+min))
    h = (if max == min then 0 else if r==max then (g-b)/d+(if g<d then 6 else 0) else if g==max then (b-r)/d+2 else (r-g)/d+4)/6
            
red, green, blue, alpha :: (Integral a) => Pixel -> a
red (Pixel pixel)   = fromIntegral $ pixel .&. 0x000000FF
green (Pixel pixel) = fromIntegral $ shiftR (pixel .&. 0x0000FF00) 8
blue (Pixel pixel)  = fromIntegral $ shiftR (pixel .&. 0x00FF0000) 16
alpha (Pixel pixel) = fromIntegral $ shiftR (pixel .&. 0xFF000000) 24

withHSL :: (HSL -> HSL) -> Pixel -> Pixel
withHSL fn pixel = let (a,b,c) = fn (getHSL pixel) in hsl a b c

shiftHue :: Double -> Pixel -> Pixel
shiftHue amount = withHSL (\ (h,s,l) -> (h + amount `mod'` 1, s, l))

brighten :: Double -> Pixel -> Pixel
brighten amount = withHSL (\ (h,s,l) -> (h, s, l + amount `mod'` 1))

shiftSaturation :: Double -> Pixel -> Pixel
shiftSaturation amount = withHSL (\ (h,s,l) -> (h, s + amount `mod'` 1, l))

hue :: Pixel -> Double
hue pixel = case getHSL pixel of (h,s,l) -> h

saturation :: Pixel -> Double
saturation pixel = case getHSL pixel of (h,s,l) -> s

luminance :: Pixel -> Double
luminance pixel = case getHSL pixel of (h,s,l) -> l

-- white = rgba 255 255 255 255
-- red   = hsl 0 100 100
-- blue  = hsl 
-- green =  

-- main = do
    -- print $ rgba 1 2 3 4
    -- print $ getRGB $ rgba 1 2 3 4
    -- print $ getRGB $ hsl 0.8 0.5 0.7
    -- print $ getHSL $ hsl 0.35 0.22 0.123
    -- print $ getRGBi (rgba 7 2 3 4 .+. rgba 1 1 1 1)
