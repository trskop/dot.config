#!/usr/bin/env stack
{- stack script
    --resolver lts-13.2
    --package directory
    --package executable-path
    --package shake
    --package time
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
-- Description: Install yx, a Command Wrapper toolset.
--              XDG Base Directory Specification
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Install yx, a Command Wrapper toolset.
module Main (main)
  where

import Control.Monad (unless)
import Data.Foldable (forM_)
import System.Exit (die)

import Data.Time.Clock.POSIX (getCurrentTime)
import System.Directory
    ( XdgDirectory(XdgConfig, XdgData)
    , getHomeDirectory
    , getXdgDirectory
    , setCurrentDirectory
    )
import System.Environment.Executable (ScriptPath(..), getScriptPath)

import Development.Shake
--import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Classes (Binary, Hashable, NFData)


newtype ThisGitRepo = ThisGitRepo ()
  deriving (Binary, Eq, Hashable, NFData, Show)

type instance RuleResult ThisGitRepo = String

thisGitRepo :: FilePath -> ThisGitRepo -> Action String
thisGitRepo directory ThisGitRepo{} = do
    Stdout status <- cmd "git status -s --"
        [ "stack.yaml"
        , "package.yaml"
        , "app-yx-env/"
        , "app-yx-new/"
        , "app-yx-path/"
        , "app-yx-this/"
        , "man/"
        ]

    if null @[] @Char status
        then do
            Stdout hash <- cmd "git -C" [directory] "show-ref -s heads/master"
            pure hash
        else
            ("Workspace dirty " <>) . show <$> liftIO getCurrentTime

data Directories = Directories
    { home :: FilePath
    , projectRoot :: FilePath
    , configDir :: FilePath
    , dataDir :: FilePath
    , localDir :: FilePath
    }

main :: IO ()
main = do
    projectRoot <- getScriptPath >>= \case
        Executable executable ->
            pure $ takeDirectory executable

        RunGHC script ->
            pure $ takeDirectory script

        Interactive ->
            die "Interactive mode not supported; call shakeMain directly."

    home <- getHomeDirectory
    configDir <- getXdgDirectory XdgConfig ""
    dataDir <- getXdgDirectory XdgData ""
    let localDir = home </> ".local"

    setCurrentDirectory projectRoot
    shakeMain Directories{..} shakeOptions

shakeMain :: Directories -> ShakeOptions -> IO ()
shakeMain Directories{..} opts = shakeArgs opts $ do
    let yxLibexecDir = localDir </> "lib" </> "yx"

        -- TODO: Time to read `package.yaml`?
        yxEnvBin = yxLibexecDir </> "yx-env"
        yxNewBin = yxLibexecDir </> "yx-new"
        yxPathBin = yxLibexecDir </> "yx-path"
        yxThisBin = yxLibexecDir </> "yx-this"
        yxJmpScript = yxLibexecDir </> "yx-jmp"

        -- Standard `man` command should be able to pick this up.  Try
        -- `manpath` after installation to be sure.
        manDir = dataDir </> "man"
        man1Dir = manDir </> "man1"
        man7Dir = manDir </> "man7"

    want
        [ yxEnvBin
        , yxNewBin
        , yxPathBin
        , yxThisBin
        , yxJmpScript
        , man1Dir </> "yx.1.gz"
        , man1Dir </> "yx-env.1.gz"
        , man1Dir </> "yx-jmp.1.gz"
        , man1Dir </> "yx-new.1.gz"
        , man1Dir </> "yx-path.1.gz"
        , man1Dir </> "yx-this.1.gz"
        ]

    hasThisRepoChanged <- addOracle (thisGitRepo projectRoot)
    [yxEnvBin, yxNewBin, yxPathBin, yxThisBin] &%> \outs -> do
        _ <- hasThisRepoChanged (ThisGitRepo ())

        let dst = takeDirectory (head outs)
        targetExists <- doesDirectoryExist dst
        unless targetExists
            $ cmd_ "mkdir -p" [dst]

        cmd_ "stack" ["--local-bin-path=" <> dst] "build --copy-bins"

    [man1Dir </> "*.1.gz", man7Dir </> "*.7.gz"] |%> \out -> do
        let tempOut = dropExtension out
            src = "man" </> dropExtension (takeFileName out) <.> "md"
            dst = takeDirectory out

        need [src]

        targetExists <- doesDirectoryExist dst
        unless targetExists
            $ cmd_ "mkdir -p" [dst]

        cmd_ "pandoc --standalone --to=man" ["--output=" <> tempOut, src]
        cmd_ "gzip --force -9" [tempOut]

    [yxJmpScript] &%> \outs ->
        forM_ outs $ \out ->
            cmd_ "ln -sf"
                [ projectRoot </> "bash" </> takeFileName out
                , takeDirectory out
                ]

-- vim:ft=haskell
