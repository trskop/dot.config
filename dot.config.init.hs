#!/usr/bin/env stack
{- stack script
    --resolver lts-12.16
    --package directory
    --package executable-path
    --package shake
    --
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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


data Directories = Directories
    { home :: FilePath
    , srcDir :: FilePath
    , configDir :: FilePath
    }

main :: IO ()
main = do
    srcDir <- getScriptPath >>= \case
        Executable exe -> pure $ takeDirectory exe
        RunGHC script -> pure $ takeDirectory script
        Interactive -> getXdgDirectory XdgConfig ""

    cacheDir <- getXdgDirectory XdgCache "dot.config"
    home <- getHomeDirectory
    configDir <- getXdgDirectory XdgConfig ""

    setCurrentDirectory home
    install Directories{..} $ shakeOptions
        { shakeFiles = cacheDir
        }

install :: Directories -> ShakeOptions -> IO ()
install Directories{..} opts = shakeArgs opts $ do
    let commandWrapperDir = configDir </> "command-wrapper"
        yxDir = configDir </> "yx"
        habitDir = configDir </> "habit"
        yxLibDir = home </> ".local" </> "lib" </> "yx"

    want
        [ home </> ".ghc/ghci.conf"
        , home </> ".haskeline"
        , home </> ".selected_editor"
        , home </> ".Xresources"
        , home </> ".psqlrc"
        , home </> ".inputrc"

        , home </> ".stack/config.yaml"
        , home </> ".stack/global-project/README.txt"
        , home </> ".stack/global-project/stack.yaml"

        , commandWrapperDir </> "default" <.> "dhall"
        , commandWrapperDir </> "command-wrapper-cd" <.> "dhall"
        , commandWrapperDir </> "command-wrapper-exec" <.> "dhall"
        , commandWrapperDir </> "command-wrapper-skel" <.> "dhall"

        , yxDir </> "default" <.> "dhall"
        , yxLibDir </> "yx-jmp"

        , habitDir </> "default.dhall"
        , habitDir </> "pgpass.conf"
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
        cmd "ln -sf" (src `dropPrefixDir` home) (takeDirectory out)

    (home </> ".haskeline") %> \out ->
        let src = (srcDir </> "haskeline" </> "prefs") `dropPrefixDir` home
        in cmd "ln -sf" src out

    (home </> ".selected_editor") %> \out ->
        let src = (srcDir </> "sensible-editor" </> "selected_editor") `dropPrefixDir` home
        in cmd "ln -sf" src out

    (home </> ".Xresources") %> \out ->
        let src = (srcDir </> "Xresources") `dropPrefixDir` home
        in cmd "ln -sf" src out

    (home </> ".psqlrc") %> \out ->
        let src = (srcDir </> "psql" </> "psqlrc") `dropPrefixDir` home
        in cmd "ln -sf" src out

    (home </> ".inputrc") %> \out ->
        let src = (srcDir </> "readline" </> "inputrc") `dropPrefixDir` home
        in cmd "ln -sf" src out

    stackRules StackRulesParams
        { home
        , srcDir
        }

    -- {{{ CommandWrapper -----------------------------------------------------

    (commandWrapperDir </> "*.dhall") %> \out -> do
        let subdir = takeBaseName out `dropPrefix` "command-wrapper-"
            dir = commandWrapperDir </> subdir
            src = dir </> "constructor.dhall"

        getDirectoryFiles dir ["*"] >>= need . map (dir </>)
        cmd (Stdin src) (FileStdout out) "dhall"

    yxRules YxRulesParams
        { configDir = yxDir
        , libDir = yxLibDir
        }

    habitRules HabitRulesParamams
        { configDir = habitDir
        }

    -- }}} CommandWrapper -----------------------------------------------------

data StackRulesParams = StackRulesParams
    { home :: FilePath
    , srcDir :: FilePath
    }

stackRules :: StackRulesParams -> Rules ()
stackRules StackRulesParams{..} = do
    (home </> ".stack/config.yaml") %> \out ->
        symlink ("stack" </> takeFileName out) out

    (home </> ".stack/global-project/README.txt") %> \out ->
        symlink ("stack" </> "global-project" </> takeFileName out) out

    (home </> ".stack/global-project/stack.yaml") %> \out ->
        symlink ("stack" </> "global-project" </> takeFileName out) out
  where
    symlink src' dst = do
        let src = srcDir </> src'

        targetExists <- doesFileExist dst
        when targetExists
          $ cmd "mv" dst (dst <.> "bac")

        cmd "ln -sf" src dst

data YxRulesParams = YxRulesParams
    { configDir :: FilePath
    , libDir :: FilePath
    }

-- | CommandWrapper toolset `yx` is used for personal tools.
yxRules :: YxRulesParams -> Rules ()
yxRules YxRulesParams{..} = do
    (configDir </> "*.dhall") %> \out -> do
        let subdir = takeBaseName out `dropPrefix` "yx-"
            dir = configDir </> subdir
            src = dir </> "constructor.dhall"

        getDirectoryFiles dir ["*"] >>= need . map (dir </>)
        cmd (Stdin src) (FileStdout out) "dhall"

    (libDir </> "yx-jmp") %> \out ->
        let src = configDir </> "toolset" </> "bash" </> "yx-jmp"
        in cmd "ln -sf" src out

newtype HabitRulesParamams = HabitRulesParamams
    { configDir :: FilePath
    }

-- | CommandWrapper toolset `habit` is used at work. Most of the configuration
-- is not actually in the repository, only the skeleton.
habitRules :: HabitRulesParamams -> Rules ()
habitRules HabitRulesParamams{..} = do
    (configDir </> "*.dhall") %> \out -> do
        let subdir = takeBaseName out `dropPrefix` "habit-"
            dir = configDir </> subdir
            src = dir </> "constructor.dhall"

        getDirectoryFiles dir ["*"] >>= need . map (dir </>)
        cmd (Stdin src) (FileStdout out) "dhall"

    -- See `psql(1)` for more details about `pgpass` file.
    (configDir </> "pgpass.conf") %> \out -> do
        let pgpassDir = configDir </> "pgpass.d"
        srcs <- map (pgpassDir </>) <$> getDirectoryFiles pgpassDir ["*"]
        if null srcs
            then
                writeFile' out "# Empty\n"
            else do
                need srcs
                () <- cmd (FileStdout out) "sed" "/^#/d" srcs
                cmd "chmod" "u=rw,go=" out

dropPrefixDir :: FilePath -> FilePath -> FilePath
dropPrefixDir path prefix = dropPrefix' path' prefix'
  where
    path' = splitPath path
    prefix' = splitPath (addTrailingPathSeparator prefix)

    dropPrefix' x        []       = joinPath x
    dropPrefix' []       _        = path  -- Prefix doesn't match.
    dropPrefix' (x : xs) (y : ys)
      | x == y                    = dropPrefix' xs ys
      | otherwise                 = path  -- Prefix doesn't match.

dropPrefix :: FilePath -> String -> FilePath
dropPrefix file prefix
  | prefix `isPrefixOf` file = drop (length prefix) file
  | otherwise                = file
