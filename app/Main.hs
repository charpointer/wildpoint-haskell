module Main where

import Control.Monad
import System.Environment (getArgs)
import API 
    ( RFC,
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

-- | Naive argument parsing, probably should replace
handleArg :: String -> IO ()
handleArg command = case command of 
    "--help" -> showHelp
    "--list" -> listRFCs
    x        -> putStrLn ("Invalid command \"" ++ x ++ "\", try --help")

main :: IO ()
main = do
    args <- getArgs

    let arg = head args
    handleArg arg
