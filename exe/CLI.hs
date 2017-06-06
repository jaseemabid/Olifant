{-|
Module      : Main
Description : The CLI interface to Olifant
-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Olifant

import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import System.IO

usage :: IO ()
usage = putStrLn "Usage: olifant [-vh] [file] "

version :: IO ()
version = putStrLn "The Glorious Olifant, version 0.0.0.1"

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = usage
parseArgs ["-v"] = version
parseArgs [file] = do
    source <- readFile file
    ll <- exec source
    writeFile (replaceExtension file ".ll") ll
parseArgs _ =
    hReady stdin >>= \case
        False -> usage
        True ->
            getContents >>= \case
                "\n" -> usage
                source -> (exec source >>= putStrLn)

main :: IO ()
main = getArgs >>= parseArgs

-- | Compile a string and return result
exec :: String -> IO String
exec str = readIO str >>= native
