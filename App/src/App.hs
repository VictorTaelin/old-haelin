module App where

import Image

data Input 
    = KeyDown    Char 
    | KeyUp      Char
    | MouseDown  Char
    | MouseUp    Char
    | MouseClick Char
    | MouseMove  Int  Int

data App st = App { 
    tick   :: Double -> st -> st,
    input  :: Input  -> st -> st,
    render :: st     -> Image}
