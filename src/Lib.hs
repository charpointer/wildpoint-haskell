module Lib
    ( printRFC
    ) where

import API (
    RFC,
    rfcFromJSON,
    rfcToJSON )

import Data.ByteString.Lazy.UTF8 as BLU (fromString)


printRFC :: RFC -> IO ()
printRFC rfc = do
    -- Parse the shit
    let shitString = BLU.fromString "{\"id\": \"1\", \"status\": \"blah\", \"date\": \"69\", \"infoProvided\": \"no\", \"lastUpdated\": \"no\"}"
    let maybeRFC = rfcFromJSON shitString
    putStrLn $ show maybeRFC