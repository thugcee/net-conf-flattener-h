{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( flatten, Error, pythonIndex, processJuniper, flattenJuniper
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal.Builder as T
import Debug.Trace

type Error = Text
data ConfType = Juniper | Cisco | UnknownConf deriving (Eq, Enum, Show)


flatten :: Text -> Either Text Error
flatten content = case confType of
    Juniper -> Left $ processJuniper [] [] confLines
    Cisco   -> Left $ processCisco confLines
    UnknownConf -> Right "Unknown configuration format"
    where confLines = T.lines content
          confType = detectType confLines


detectType :: [Text] -> ConfType
detectType confLines
    | isJunOs thirdLn  = Juniper
    | isCisco lastLn   = Cisco
    | otherwise        = UnknownConf
    where thirdLn = if length confLines >= 3 then confLines !! 2 else ""
          lastLn = last confLines


isJunOs :: Text -> Bool
isJunOs thirdLn = thirdLn == "system {" || thirdLn == "groups {"


isCisco :: Text -> Bool
isCisco lastLn = lastLn == "end"


processCisco :: [Text] -> Text
processCisco lines = "Cisco"


processJuniper :: [Text] -> [Text] -> [Text] -> Text
processJuniper flat context [] = T.unlines $ reverse flat
processJuniper flat context [last] = processJuniper (last:flat) [] []
processJuniper flat context structural =
    let (flatStatement, rest) = flattenJuniper context structural
        (statement:section) = flatStatement
        joinedStatement = T.intercalate " " $ reverse flatStatement
    in
        processJuniper (joinedStatement:flat) section rest

flattenJuniper :: [Text] -> [Text] -> ([Text], [Text])
flattenJuniper context [] = (context, [])
flattenJuniper context [last] = (last:context, [])
flattenJuniper context (first:rest) =
    case pythonIndex 1 first of
        "#" -> (first:context, rest)
        _ -> case pythonIndex (-1) first of
            "{" -> flattenJuniper (T.strip (T.dropEnd 1 first):context) rest
            ";" -> (T.strip first:context, rest)
            "}" -> let (_:contextUp) = context in flattenJuniper contextUp rest
            _ -> error "parsing error"


pythonIndex :: Int -> Text -> Text
pythonIndex n text
    | n < 0 = T.takeEnd (-n) text
    | n > 0 = T.take n text
    | otherwise = ""