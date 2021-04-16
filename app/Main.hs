module Main where

import System.Environment (getArgs)
import API 
    ( RFC,
      getAllRFCS,
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
    rfcs <- getAllRFCS 

    putStrLn $ show rfcs


handleArg :: String -> IO ()
handleArg "--help" = showHelp
handleArg "--list" = listRFCs
handleArg _ = putStrLn "Unknown command"

main :: IO ()
main = do
    args <- getArgs

    let arg = head args
    handleArg arg
