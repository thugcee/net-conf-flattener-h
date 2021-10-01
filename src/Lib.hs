module Lib
    ( flatten
    ) where

data ConfType = Juniper | Cisco | UnknownConf deriving (Eq, Enum, Show)


flatten :: String -> String
flatten content = case confType of
    Juniper -> processJuniper confLines
    Cisco   -> processCisco confLines
    UnknownConf -> ""
    where confLines = lines content
          confType = detectType confLines


detectType :: [String] -> ConfType
detectType confLines
    | thirdLn == "system {" || thirdLn == "groups {"    = Juniper
    | lastLn == "end"                                   = Cisco
    | otherwise                                         = UnknownConf
    where thirdLn = if length confLines >= 3 then confLines !! 2 else ""
          lastLn = last confLines


processCisco :: [String] -> String
processCisco = error "not implemented"


processJuniper :: [String] -> String
processJuniper = error "not implemented"