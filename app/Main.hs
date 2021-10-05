module Main where

import System.IO
import System.Environment
import Lib

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


main :: IO ()
main = getArgs >>= mapM_ (loadAndProcess flatten)


loadAndProcess :: (Text -> Either Text Lib.Error) -> String -> IO ()
loadAndProcess f "-" = T.hGetContents System.IO.stdin >>= process f
loadAndProcess f filename = withFile filename ReadMode (\handle -> T.hGetContents handle >>= process f)


process :: (Text -> Either Text Lib.Error) -> Text -> IO ()
process f text =  case f text of
   Left text2 -> putStrLn $ T.unpack text2
   Right errorMsg -> hPutStrLn stderr $ T.unpack errorMsg
