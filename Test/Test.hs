{-# LANGUAGE ViewPatterns #-}

import App
import App.Terminal
import Debug.Trace
import Linear
import Range
import Terminal
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import qualified Image as I

data Game = Game {
    time :: Double,
    pos  :: V2 Double}
    deriving Show

app :: App Game
app = App { start = start, tick = tick, input = input, render = renderFn } where 
    start              = Game 0 (V2 64 64)
    tick dt (Game t p) = Game (t+dt) p
    input (KeyDown key) game@(Game t p) = Game t $ case key of
        'd' -> p ^+^ V2 6 0
        'a' -> p ^-^ V2 6 0
        'w' -> p ^+^ V2 0 6
        's' -> p ^-^ V2 0 6
    input _ g = g

renderFn :: Game -> I.Image
renderFn (Game t p) = I.draw drawings baseImage where
    squareColor = I.hsl 0.5 0.5 0.5
    baseImage   = I.replicateDeep (V2 256 256) (I.rgba 255 255 255 255)
    drawings    = I.rect squareColor fromPos toPos 0
    centerPos   = p ^+^ particlePos
    particlePos = V2 (cos (t*4)) (sin (t*4)) ^* 0
    fromPos     = fmap floor $ centerPos ^-^ V2 16 16
    toPos       = fmap floor $ centerPos ^+^ V2 16 16

main = do
    game <- runApp app
    putStrLn $ "App ran for " ++ show game ++ "s"



-- clear
-- I.printImageTop (renderFn 0.1)
-- print $ I.getRGB (I.hsl 0.0 0 0)
-- main = runApp app 0


    -- let path  = "/Users/v/hs/Pokemon/Image/8736.png"
    -- image     <- I.load path
    -- let loop = do
            -- time      <- now
            -- let pos    = fmap floor $ V2 32 32 ^+^ V2 (cos time) (sin time) ^* 16
            -- let image' = I.draw (I.blit pos graySquare) image
            -- I.printImageTop image'
            -- delay 200
            -- loop
    -- loop
