module Main where

import System.IO
import System.Environment
import Lib

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


main :: IO ()
main = getArgs >>= mapM_ (loadArgFileName >=> display . flatten)


loadArgFileName :: String -> IO Text
loadArgFileName "-" = T.hGetContents System.IO.stdin
loadArgFileName filename = T.readFile filename


display :: Either Text Lib.Error -> IO ()
display result =  case result of
   Left text2 -> putStrLn $ T.unpack text2
   Right errorMsg -> hPutStrLn stderr $ T.unpack errorMsg
