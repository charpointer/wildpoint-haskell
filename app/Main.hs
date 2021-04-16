module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs

    putStrLn ("Shit: " ++ show args)