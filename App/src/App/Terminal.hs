{-# LANGUAGE ScopedTypeVariables #-}

module App.Terminal where

import App
import Control.Concurrent.Thread.Delay
import Control.Monad (when)
import Data.IORef
import Image
import System.IO
import Terminal

runApp :: forall state . (Show state) => App state -> IO state
runApp app = do
    stateRef <- newIORef (start app) :: IO (IORef state)
    hSetBuffering stdin NoBuffering

    let loop = do

            -- Update state
            modifyIORef stateRef (tick app (1/60))
            hasInput   <- hReady stdin
            pressedKey <- if hasInput 
                then fmap Just getChar
                else return Nothing
            case pressedKey of
                Just key -> modifyIORef stateRef . input app . KeyDown $ key
                Nothing  -> return ()

            -- Render state
            state <- readIORef stateRef
            printImageTop (render app state)

            -- Loops after 1/60s
            delay (div 1000 60) 
            case pressedKey of
                Just '\ESC' -> return state 
                _           -> loop

    clear
    loop


-- That's a much better way to do it, from EvanR on #haskell,
-- using threads (which I still didn't study). He creates 2
-- concurrent threads, one responsible for getting the char
-- inputs, other attempts to get the character it holds with
-- `tryTakeMVar` which doesn't block. Very cool! I need to
-- incorporate that.

-- charGetter :: MVar Char -> IO a
-- charGetter out = forever $ do
  -- c <- getChar
  -- putMVar out c

-- mainLoop :: IORef S -> MVar Char -> IO ()
-- mainLoop sref inChar = do
  -- mc <- tryTakeMVar inChar
  -- case mc of
    -- Just c -> do
      -- (modifyIORef srf . something) key
      -- (printImageTop . render something) =<< readIORef sref
      -- when (c=='\ESC') $ do
        -- delay (div 1000 60)
        -- mainLoop sref inChar
    -- Nothing -> do
      -- (printImageTop . render something) =<< readIORef sref
      -- delay (div 1000 60)
      -- mainLoop sref inChar

-- main :: IO a
-- main = do
  -- hSetBuffering stdin NoBuffering
  -- ref <- newIORef newS
  -- mv <- newEmptyMVar
  -- forkIO (charGetter mv)
  -- mainLoop ref mv
