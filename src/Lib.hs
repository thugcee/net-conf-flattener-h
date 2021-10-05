{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( flatten, Error, pythonIndex, processJuniper, getNextStatement
    ) where

import Data.Text (Text)
import qualified Data.Text as T

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
processCisco _ = "Cisco"


processJuniper :: [Text] -> [Text] -> [Text] -> Text
processJuniper flat _ [] = T.unlines $ reverse flat
processJuniper flat _ [lastLn] = processJuniper (lastLn:flat) [] []
processJuniper flat context structural =
    let (flatStatement, rest) = getNextStatement context structural
        (_:section) = flatStatement
        joinedStatement = T.intercalate " " $ reverse flatStatement
    in
        processJuniper (joinedStatement:flat) section rest


getNextStatement :: [Text] -> [Text] -> ([Text], [Text])
getNextStatement context [] = (context, [])
getNextStatement context [lastLn] = (lastLn:context, [])
getNextStatement context (first:rest) =
    case pythonIndex 1 first of
        "#" -> (first:context, rest)
        _ -> case pythonIndex (-1) first of
            "{" -> getNextStatement (T.strip (T.dropEnd 1 first):context) rest
            ";" -> (T.strip first:context, rest)
            "}" -> let (_:contextUp) = context in getNextStatement contextUp rest
            _ -> error "parsing error"


pythonIndex :: Int -> Text -> Text
pythonIndex n text
    | n < 0 = T.takeEnd (-n) text
    | n > 0 = T.take n text
    | otherwise = ""
