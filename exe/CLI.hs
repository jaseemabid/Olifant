{-|
Module      : Main
Description : The CLI interface to Olifant
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude   (String)
import Protolude hiding (cast)

import Olifant.Compiler
import Olifant.Parser

import System.Environment (getArgs)
import System.FilePath    (replaceExtension)
import System.IO          (hReady)

usage :: IO ()
usage = putStrLn ("Usage: olifant [-vh] [file] " :: Text)

version :: IO ()
version = putStrLn ("The Glorious Olifant, version 0.0.0.1" :: Text)

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
exec :: Text -> IO Text
exec str = do
  let Right a = parse str
  case compile a of
      Right ut -> return $ show ut
      Left e   -> return $ show e
