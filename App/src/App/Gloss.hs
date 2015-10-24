{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module App.Gloss where

import App
import GHC.Float (float2Double)
import Image (Image(..), toREPA, getRGB)
import Linear (V2(..))
import qualified Data.Array.Repa as R (map)
import qualified Graphics.Gloss as G (Picture)
import qualified Graphics.Gloss.Interface.Pure.Game as GM (Event (EventKey), Key (Char), KeyState (Down, Up))
import qualified Graphics.Gloss.Raster.Array as G (playArray, Display (InWindow), rgbI)

runApp :: forall state . App state -> state -> IO ()
runApp app state = G.playArray display (1,1) 60 state glossRender glossEvent glossTick where

    (Image (V2 w h) _ _) = render app state

    display :: G.Display
    display = G.InWindow "λApp" (w,h) (0,0)

    glossTick :: Float -> state -> state
    glossTick deltaTime = tick app (float2Double deltaTime)

    -- glossRender :: state -> G.Picture
    glossRender = R.map toGlossPixel . toREPA . render app where
        f                              = fromIntegral
        toGlossPixel (getRGB->(r,g,b)) = G.rgbI (f r) (f g) (f b)

    glossEvent :: GM.Event -> state -> state 
    glossEvent (GM.EventKey (GM.Char key) GM.Down _ _) = input app (KeyDown key)
    glossEvent (GM.EventKey (GM.Char key) GM.Up _ _)   = input app (MouseDown key)
    -- TODO: implement mouse inputs
    glossEvent _                                       = id
