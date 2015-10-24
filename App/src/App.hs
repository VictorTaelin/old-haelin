module App where

import Image

data Input 
    = KeyDown    Char 
    | KeyUp      Char
    | MouseDown  Char
    | MouseUp    Char
    | MouseClick Char
    | MouseMove  Int  Int

data App state = App { 
    start  :: state,
    tick   :: Double -> state -> state,
    input  :: Input  -> state -> state,
    render :: state  -> Image}
