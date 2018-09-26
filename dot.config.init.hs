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
-- Description: Initialise configuration for applications that do not support
--              XDG Base Directory Specification
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Initialise configuration for applications that do not support XDG Base
-- Directory Specification.
module Main (main)
  where

import Control.Monad (unless, when)
import System.Directory
    ( XdgDirectory(XdgCache, XdgConfig)
    , getHomeDirectory
    , getXdgDirectory
    , setCurrentDirectory
    )
import qualified System.Directory as Directory (doesDirectoryExist)
import System.Environment.Executable (ScriptPath(..), getScriptPath)

import Development.Shake
import Development.Shake.FilePath


main :: IO ()
main = do
    srcDir <- getScriptPath >>= \case
        Executable exe -> pure $ takeDirectory exe
        RunGHC script -> pure $ takeDirectory script
        Interactive -> getXdgDirectory XdgConfig ""

    cacheDir <- getXdgDirectory XdgCache "dot.config"
    homeDir <- getHomeDirectory

    setCurrentDirectory homeDir
    install homeDir srcDir $ shakeOptions
        { shakeFiles = cacheDir
        }

install :: FilePath -> FilePath -> ShakeOptions -> IO ()
install home srcDir opts = shakeArgs opts $ do
    want
        [ home </> ".ghc/ghci.conf"
        , home </> ".haskeline"
        , home </> ".selected_editor"
        , home </> ".Xresources"
        , home </> ".psqlrc"
        ]

    (home </> ".ghc/ghci.conf") %> \out -> do
        let src = srcDir </> "ghc"
            dst = (takeDirectory out)
        targetExists <- doesDirectoryExist dst
        -- TODO: If target is a symbolic link, then it should be removed
        --       instead of renamed, since there will be no permanent
        --       information loss.
        when targetExists
          $ cmd "mv" dst (takeDirectory dst </> ".ghc-bac")
        () <- cmd "chmod 700" src
        cmd "ln -sf" (src `dropPrefix` home) (takeDirectory out)

    (home </> ".haskeline") %> \out ->
        let src = (srcDir </> "haskeline" </> "prefs") `dropPrefix` home
        in cmd "ln -sf" src out

    (home </> ".selected_editor") %> \out ->
        let src = (srcDir </> "sensible-editor" </> "selected_editor") `dropPrefix` home
        in cmd "ln -sf" src out

    (home </> ".Xresources") %> \out ->
        let src = (srcDir </> "Xresources") `dropPrefix` home
        in cmd "ln -sf" src out

    (home </> ".psqlrc") %> \out ->
        let src = (srcDir </> "psql" </> "psqlrc") `dropPrefix` home
        in cmd "ln -sf" src out

dropPrefix :: FilePath -> FilePath -> FilePath
dropPrefix path prefix = dropPrefix' path' prefix'
  where
    path' = splitPath path
    prefix' = splitPath (addTrailingPathSeparator prefix)

    dropPrefix' x        []       = joinPath x
    dropPrefix' []       _        = path  -- Prefix doesn't match.
    dropPrefix' (x : xs) (y : ys)
      | x == y                    = dropPrefix' xs ys
      | otherwise                 = path  -- Prefix doesn't match.
