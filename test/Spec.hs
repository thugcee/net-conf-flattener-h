{-# LANGUAGE OverloadedStrings #-}
import Lib

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal.Builder as T

main :: IO ()
main = do
    putStrLn "\nTests:"
    print $ flattenJuniper ["system"] ["elmo;"]
    print $ flattenJuniper ["system"] ["elmo;","gaza;"]
    print $ flattenJuniper ["elmo;","system"] ["gaza;"]
    let (context, rest) = flattenJuniper [] ["system {"
                                                        , "    host-name ex-aci;"
                                                        , "    name-server {"
                                                        , "        212.14.1.62;"
                                                        , "        212.14.1.66;"
                                                        , "    }"
                                                        , "    auto-snapshot;"
                                                        , "}"
                                                        ]
        in do
            print (reverse context, rest)
            let (context2, rest2) = flattenJuniper [(head context)] rest
                in do
                    print (reverse context2, rest2)
    putStrLn "Done."
