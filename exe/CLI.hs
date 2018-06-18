{-|
Module      : Main
Description : The CLI interface to Olifant
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude   (String)
import Protolude hiding (mod, sin)

import Olifant.Compiler
import Olifant.Core
import Olifant.Gen      (gen)
import Olifant.Parser

import System.Environment (getArgs)
import System.FilePath    (takeBaseName)
import System.IO          (hReady)
import System.Process     (callCommand)


usage :: IO ()
usage = putStrLn ("Usage: olifant [-vhcp] [file] " :: Text)

version :: IO ()
version = putStrLn ("The Glorious Olifant, version 0.0.0.1" :: Text)

-- | Compile source to core
core :: Text -> Either Error [Core]
core src = parse src >>= compile

-- | Generate native code from source
exec :: Text -> IO (Either Error Text)
exec str =
    case core str of
        Right prog -> do
            mod <- gen prog
            case mod of
                Right native -> return $ Right native
                Left e       -> return $ Left e
        Left e -> return $ Left e

-- | Generate an executable from the generated llvm IR.
--
-- This is fairly messy and could be refactored a bit.
--
-- 1. Directly using `make` could delete a lot of duplicated code.
-- 2. Use mktemp for the temp files once
--    https://github.com/haskell/unix/issues/112 gets fixed.
-- 3. Cleanup temp files after completion.
--
binary :: Text -> IO ()
binary ll = do
    writeFile tmp ll
    callCommand cmd
    callCommand ofile

  where
    -- Keep this in sync with the Makefile
    runtime = "runtime/olifant.o"
    ccflags = "-g -ggdb3 -m64 -Wall -Wpedantic -fno-asynchronous-unwind-tables -Wno-override-module"
    ofile   = "/tmp/cmd.exe"

    tmp = "/tmp/Olifant.XXX.ll"
    cmd = "clang " ++ ccflags ++ " " ++ runtime ++ " " ++ tmp ++ " -o " ++ ofile

-- | Get the input from a file or stdin
sin :: [String] -> IO (Maybe Text)
sin [file] = Just <$> readFile file
sin _ = hReady stdin >>= \case
    False -> return Nothing
    True ->
        getContents >>= \case
        "\n" -> return Nothing
        source -> return $ Just source

parseArgs :: [String] -> IO ()

parseArgs ("-h": _) = usage
parseArgs ("-v": _) = version

-- Dump LLVM IR
parseArgs ("-l": args) =
    sin args >>= \case
        Just src -> do
            ll <- exec src
            case ll of
                Right t ->
                    case args of
                        [file] -> writeFile (takeBaseName file ++ ".ll") t
                        _ -> putStrLn t
                Left e  -> putStrLn $ render e
        Nothing -> usage

-- Dump core, pipe to hindent to make the output readable
parseArgs ("-c": args) =
    sin args >>= \case
        Just src ->
            case core src of
                Right t -> print t
                Left e  -> putStrLn $ render e
        Nothing -> usage

-- Dump parser output, pipe to hindent to make the output readable
parseArgs ("-p": args) =
    sin args >>= \case
        Just src ->
            case parse src of
                Right t -> print t
                Left e  -> putStrLn $ render e
        Nothing -> usage

parseArgs args =
    sin args >>= \case
        Just src ->
            exec src >>= \case
                Right ir -> binary ir
                Left err -> putStrLn $ render err
        Nothing -> usage

main :: IO ()
main = getArgs >>= parseArgs
