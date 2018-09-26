#!/usr/bin/env stack
{- stack script
    --resolver lts-12.7
    --package directory
    --package executable-path
    --package shake
    --
-}

{-# LANGUAGE LambdaCase #-}

-- |
-- Module:      Main
-- Description: Build CommandWrapper configuration.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Build CommandWrapper configuration.
module Main (main)
  where

import Control.Monad (unless, when)
import Data.List (isPrefixOf)

import System.Directory
    ( XdgDirectory(XdgCache, XdgConfig)
    , getHomeDirectory
    , getXdgDirectory
    , setCurrentDirectory
    )
import qualified System.Directory as Directory (doesDirectoryExist)
import System.Environment.Executable (ScriptPath(..), getScriptPath)

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath


main :: IO ()
main = do
    srcDir <- getScriptPath >>= \case
        Executable exe -> pure $ takeDirectory exe
        RunGHC script -> pure $ takeDirectory script
        Interactive -> getXdgDirectory XdgConfig "command-wrapper"

    cacheDir <- getXdgDirectory XdgCache "dot.config/command-wrapper"

    setCurrentDirectory srcDir
    install $ shakeOptions
        { shakeFiles = cacheDir
        }

install :: ShakeOptions -> IO ()
install opts = shakeArgs opts $ do

    want
        [ "default.dhall"
        , "command-wrapper-cd.dhall"
        , "command-wrapper-exec.dhall"
        , "command-wrapper-skel.dhall"
        ]

    "*.dhall" %> \out -> do
        let dir = takeBaseName (out `dropPrefix` "command-wrapper-")
            src = "." </> dir </> "constructor.dhall"
        getDirectoryFiles dir ["*"] >>= need . map (dir </>)
        cmd (Stdin src) (FileStdout out) "dhall"

dropPrefix :: FilePath -> String -> FilePath
dropPrefix file prefix
  | prefix `isPrefixOf` file = drop (length prefix) file
  | otherwise                = file
