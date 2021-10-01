module Main where

import System.Exit
import System.Environment
import Lib

main :: IO ()
main = getArgs >>= mapM_ processFile

processFile :: String -> IO ()
processFile filename = load filename >>= (putStrLn . flatten)

load :: String -> IO String
load "-" = getContents
load filename = readFile filename