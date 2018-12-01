#!/usr/bin/env stack
{- stack script
    --resolver lts-12.20
    --package directory
    --package executable-path
    --package shake
    --
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

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
import GHC.TypeLits (Symbol)

import System.Directory
    ( XdgDirectory(XdgCache, XdgConfig)
    , getHomeDirectory
    , getXdgDirectory
    , pathIsSymbolicLink
    , setCurrentDirectory
    )
import System.Environment.Executable (ScriptPath(..), getScriptPath)

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes (Binary, Hashable, NFData)


newtype GitRepo (name :: Symbol) = GitRepo ()
  deriving (Binary, Eq, Hashable, NFData, Show)

data GitRepoConfig (name :: Symbol) = GitRepoConfig
    { directory :: FilePath
    , url :: String
    }
  deriving (Show)

gitRepo :: GitRepoConfig name -> GitRepo name -> Action String
gitRepo GitRepoConfig{..} GitRepo{} = do
    repoExists <- doesDirectoryExist directory
    unless repoExists
        $ cmd "git clone" url directory
    cmd_ "git -C" [directory] "fetch --all"
    Stdout hash <- cmd "git -C" [directory] "show-ref -s origin/HEAD"
    pure hash

type instance RuleResult (GitRepo "github.com/trskop/command-wrapper") = String

mkCommandWrapperRepoConfig
    :: FilePath
    -> GitRepoConfig "github.com/trskop/command-wrapper"
mkCommandWrapperRepoConfig dotLocalDir = GitRepoConfig
    { directory = dotLocalDir </> "src" </> "trskop" </> "command-wrapper"
    , url = "https://github.com/trskop/command-wrapper.git"
    }

type instance RuleResult (GitRepo "github.com/trskop/genbashrc") = String

mkGenbashrcRepoConfig :: FilePath -> GitRepoConfig "github.com/trskop/genbashrc"
mkGenbashrcRepoConfig dotLocalDir = GitRepoConfig
    { directory = dotLocalDir </> "src" </> "trskop" </> "genbashrc"
    , url = "https://github.com/trskop/genbashrc.git"
    }

type instance RuleResult (GitRepo "github.com/powerline/fonts") = String

mkPowerlineFontsRepoConfig
    :: FilePath
    -> GitRepoConfig "github.com/powerline/fonts"
mkPowerlineFontsRepoConfig dotLocalDir = GitRepoConfig
    { directory = dotLocalDir </> "src" </> "powerline" </> "fonts"
    , url = "https://github.com/powerline/fonts.git"
    }

type instance RuleResult (GitRepo "github.com/junegunn/fzf") = String

mkFzfRepoConfig :: FilePath -> GitRepoConfig "github.com/junegunn/fzf"
mkFzfRepoConfig dotLocalDir = GitRepoConfig
    { directory = dotLocalDir </> "src" </> "junegunn" </> "fzf"
    , url = "https://github.com/junegunn/fzf.git"
    }

data Directories = Directories
    { home :: FilePath
    , srcDir :: FilePath
    , configDir :: FilePath
    }

main :: IO ()
main = do
    srcDir <- getScriptPath >>= \case
        Executable executable -> pure $ takeDirectory executable
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
        binDir = home </> "bin"
        yxDir = configDir </> "yx"
        habitDir = configDir </> "habit"
        dotLocalDir = home </> ".local"
        yxLibDir = dotLocalDir </> "lib" </> "yx"
        commandWrapperLibDir = dotLocalDir </> "lib" </> "command-wrapper"
        commandWrapperRepoConfig = mkCommandWrapperRepoConfig dotLocalDir
        genbashrcRepoConfig = mkGenbashrcRepoConfig dotLocalDir
        powerlineFontsRepoConfig = mkPowerlineFontsRepoConfig dotLocalDir
        fzfRepoConfig = mkFzfRepoConfig dotLocalDir

        dejaVuSansMonoPowerlineTtf =
            dotLocalDir </> "share" </> "fonts"
            </> "DejaVu Sans Mono for Powerline.ttf"

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
        , binDir </> "stack-help"

        , dejaVuSansMonoPowerlineTtf

        , dotLocalDir </> "bin" </> "genbashrc"

        , commandWrapperLibDir </> "command-wrapper"
        , commandWrapperDir </> "default" <.> "dhall"
        , commandWrapperDir </> "command-wrapper-cd" <.> "dhall"
        , commandWrapperDir </> "command-wrapper-exec" <.> "dhall"
        , commandWrapperDir </> "command-wrapper-skel" <.> "dhall"

        , binDir </> "yx"
        , yxDir </> "default" <.> "dhall"
        , yxLibDir </> "yx-jmp"

        , habitDir </> "default.dhall"
        , habitDir </> "pgpass.conf"

        , binDir </> "fzf"
        ]

    (home </> ".ghc/ghci.conf") %> \out -> do
        let src = srcDir </> "ghc"
            dst = (takeDirectory out)
        targetExists <- doesDirectoryExist dst
        when targetExists $ do
            targetIsSymlink <- liftIO (pathIsSymbolicLink dst)
            unless targetIsSymlink
                $ cmd "mv" [dst, takeDirectory dst </> ".ghc-bac"]
        cmd_ "chmod 700" src
        cmd_ "ln -sf" [src `dropPrefixDir` home, takeDirectory out]

    (home </> ".haskeline") %> \out ->
        let src = (srcDir </> "haskeline" </> "prefs") `dropPrefixDir` home
        in symlink src out

    (home </> ".selected_editor") %> \out ->
        let src = (srcDir </> "sensible-editor" </> "selected_editor") `dropPrefixDir` home
        in symlink src out

    (home </> ".Xresources") %> \out ->
        let src = (srcDir </> "Xresources") `dropPrefixDir` home
        in symlink src out

    (home </> ".psqlrc") %> \out ->
        let src = (srcDir </> "psql" </> "psqlrc") `dropPrefixDir` home
        in symlink src out

    (home </> ".inputrc") %> \out ->
        let src = (srcDir </> "readline" </> "inputrc") `dropPrefixDir` home
        in symlink src out

    stackRules StackRulesParams
        { home
        , srcDir = srcDir </> "stack"
        , binDir
        }

    genbashrcUpToDate <- addOracle (gitRepo genbashrcRepoConfig)

    (dotLocalDir </> "bin" </> "genbashrc") %> \_ -> do
        _ <- genbashrcUpToDate (GitRepo ())
        let GitRepoConfig{directory} = genbashrcRepoConfig
        cmd_ "git -C" directory "pull"
        cmd_ "stack --stack-yaml" (directory </> "stack.yaml") "install"

    powerlineFontsUpToDate <- addOracle (gitRepo powerlineFontsRepoConfig)

    dejaVuSansMonoPowerlineTtf %> \_ -> do
        _ <- powerlineFontsUpToDate (GitRepo ())
        let GitRepoConfig{directory} = powerlineFontsRepoConfig
        cmd_ "git -C" directory "pull"
        cmd_ (Cwd directory) "./install.sh"

    -- {{{ CommandWrapper -----------------------------------------------------

    commandWrapperUpToDate <- addOracle (gitRepo commandWrapperRepoConfig)

    (commandWrapperLibDir </> "command-wrapper") %> \_ -> do
        _ <- commandWrapperUpToDate (GitRepo ())
        let GitRepoConfig{directory} = commandWrapperRepoConfig
        cmd_ "git -C" [directory] "pull"
        cmd_ (Cwd directory) "./install"

    (commandWrapperDir </> "*.dhall") %> \out -> do
        need [commandWrapperLibDir </> "command-wrapper"]
        let subdir = takeBaseName out `dropPrefix` "command-wrapper-"
            dir = commandWrapperDir </> subdir
            src = dir </> "constructor.dhall"

        getDirectoryFiles dir ["*"] >>= need . map (dir </>)
        cmd_ (Stdin src) (FileStdout out) "dhall"

    yxRules YxRulesParams
        { configDir = yxDir
        , libDir = yxLibDir
        , binDir
        , commandWrapperLibDir
        }

    habitRules HabitRulesParamams
        { configDir = habitDir
        }

    -- }}} CommandWrapper -----------------------------------------------------

    fzfRepoUpToDate <- addOracle (gitRepo fzfRepoConfig)

    (binDir </> "fzf") %> \out -> do
        _ <- fzfRepoUpToDate (GitRepo ())
        let GitRepoConfig{directory} = fzfRepoConfig
        cmd_ (directory </> "install") "--all" "--xdg" "--no-update-rc"
        symlink (directory </> "bin" </> takeFileName out) out

data StackRulesParams = StackRulesParams
    { home :: FilePath
    , srcDir :: FilePath
    , binDir :: FilePath
    }

stackRules :: StackRulesParams -> Rules ()
stackRules StackRulesParams{..} = do
    (home </> ".stack/config.yaml") %> \out ->
        symlink (srcDir </> takeFileName out) out

    (home </> ".stack/global-project/README.txt") %> \out ->
        symlink (srcDir </> "global-project" </> takeFileName out) out

    (home </> ".stack/global-project/stack.yaml") %> \out ->
        symlink (srcDir </> "global-project" </> takeFileName out) out

    (binDir </> "stack-help") %> \out ->
        symlink (srcDir </> "bin" </> takeFileName out) out

symlink :: FilePath -> FilePath -> Action ()
symlink src dst = do
    targetExists <- doesFileExist dst
    when targetExists $ do
        targetIsSymlink <- liftIO (pathIsSymbolicLink dst)
        unless targetIsSymlink
            $ cmd "mv" [dst, dst <.> "bac"]

    cmd_ "ln -sf" [src, dst]

data YxRulesParams = YxRulesParams
    { configDir :: FilePath
    , libDir :: FilePath
    , binDir :: FilePath
    , commandWrapperLibDir :: FilePath
    }

-- | CommandWrapper toolset `yx` is used for personal tools.
yxRules :: YxRulesParams -> Rules ()
yxRules YxRulesParams{..} = do
    (configDir </> "*.dhall") %> \out -> do
        let subdir = takeBaseName out `dropPrefix` "yx-"
            dir = configDir </> subdir
            src = dir </> "constructor.dhall"

        getDirectoryFiles dir ["*"] >>= need . map (dir </>)
        cmd_ (Stdin src) (FileStdout out) "dhall"

    (libDir </> "yx-jmp") %> \out ->
        let src = configDir </> "toolset" </> "bash" </> "yx-jmp"
        in symlink src out

    (binDir </> "yx") %> \out ->
        let src = commandWrapperLibDir </> "command-wrapper"
        in symlink src out

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
        cmd_ (Stdin src) (FileStdout out) "dhall"

    -- See `psql(1)` for more details about `pgpass` file.
    (configDir </> "pgpass.conf") %> \out -> do
        let pgpassDir = configDir </> "pgpass.d"
        srcs <- map (pgpassDir </>) <$> getDirectoryFiles pgpassDir ["*"]
        if null srcs
            then
                writeFile' out "# Empty\n"
            else do
                need srcs
                cmd_ (FileStdout out) "sed" ["/^#/d"] srcs
                cmd_ "chmod" "u=rw,go=" [out]

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
