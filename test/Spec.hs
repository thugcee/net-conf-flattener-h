{-# LANGUAGE OverloadedStrings #-}
import Lib


main :: IO ()
main = do
    putStrLn "\nTests:"
    print $ getNextStatement ["system"] ["elmo;"]
    print $ getNextStatement ["system"] ["elmo;","gaza;"]
    print $ getNextStatement ["elmo;","system"] ["gaza;"]
    let (context, rest) = getNextStatement [] ["system {"
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
            let (context2, rest2) = getNextStatement [head context] rest
                in do
                    print (reverse context2, rest2)
    putStrLn "Done."
