-- This defines some utils for working with iTerm2 on OSX

module Terminal where

import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

putBase64Image :: B.ByteString -> IO ()
putBase64Image = B.putStr . iTermBase64String

putBase64ImageLn :: B.ByteString -> IO ()
putBase64ImageLn = B.putStrLn . iTermBase64String

putBase64ImageTop :: B.ByteString -> IO ()
putBase64ImageTop str = deepseq str (moveCursorToTop >> putBase64Image str)

putBase64ImageTopLn :: B.ByteString -> IO ()
putBase64ImageTopLn str = deepseq str (moveCursorToTop >> putBase64ImageLn str)
putBase64ImageTopLn str = deepseq str (moveCursorToTop >> putBase64ImageLn str)

iTermBase64String :: B.ByteString -> B.ByteString
iTermBase64String imageBase64String = B.concat [
    B8.pack "\027]1337;File=inline=1;width=auto;height=auto:",
    imageBase64String,
    B8.pack "\7"]

clear :: IO ()
clear = putStrLn "\027[2J"

moveCursorToTop :: IO ()
moveCursorToTop = putStrLn "\027[0f"
