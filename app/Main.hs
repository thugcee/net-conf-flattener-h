module Main where

import System.Exit
import System.IO
import System.Environment
import Lib

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


main :: IO ()
main = getArgs >>= mapM_ (\arg -> loadAndProcess flatten arg)


loadAndProcess :: (Text -> Either Text Lib.Error) -> String -> IO ()
loadAndProcess f "-" = T.hGetContents System.IO.stdin >>= process f
loadAndProcess f filename = withFile filename ReadMode (\handle -> T.hGetContents handle >>= process f)


process :: (Text -> Either Text Lib.Error) -> Text -> IO ()
process f text =  case f text of
   Left text -> putStrLn $ T.unpack text
   Right error -> hPutStrLn stderr $ T.unpack error