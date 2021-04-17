module Main where
import Control.Monad
import System.Environment (getArgs)

import API 
    ( RFC,
      getRFC,
      getAllRFCS,
      printRFC,
      rfcFromJSON,
      rfcToJSON  )

showHelp :: IO ()
showHelp = do
    putStrLn "Help for wildptr-haskell"
    putStrLn "--help: shows this message (duh)"
    putStrLn "--list: lists all available RFCs"

listRFCs :: IO ()
listRFCs = do
    putStrLn "Here are a list of all available RFCs"
    mapM_ printRFC =<< getAllRFCS

main :: IO ()
main = do
    args <- getArgs
    let command = head args

    case command of
        "--help" -> showHelp
        "--list" -> listRFCs
        "--id"   -> rIO >>= \r -> case r of
                        Just r -> printRFC r
                        Nothing -> putStrLn "No RFC found!"
                    where rIO = getRFC $ args !! 1

        -- Shorthand
        "-l"     -> listRFCs
        "-h"     -> showHelp

        -- Unknown command
        x        -> putStrLn ("Invalid command \"" ++ x ++ "\", try --help")