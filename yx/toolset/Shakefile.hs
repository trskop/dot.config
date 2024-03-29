#!/usr/bin/env stack
{- stack script
    --resolver lts-18.5
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
import Data.Functor ((<&>))
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
        [ "Shakefile.hs"
        , "app-yx-env/"
        , "app-yx-new/"
        , "app-yx-path/"
        , "app-yx-remarkable/"
        , "app-yx-template/"
        , "app-yx-this/"
        , "bash/"
        , "man/"
        , "package.yaml"
        , "stack.yaml"
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

        -- Standard `man` command should be able to pick this up.  Try
        -- `manpath` after installation to be sure.
        manDir = dataDir </> "man"
        man1Dir = manDir </> "man1"
        man7Dir = manDir </> "man7"

        -- TODO: Time to read `package.yaml`?
        yxEnvBin = yxLibexecDir </> "yx-env"
        yxNewBin = yxLibexecDir </> "yx-new"
        yxPathBin = yxLibexecDir </> "yx-path"
        yxRemarkableBin = yxLibexecDir </> "yx-remarkable"
        yxThisBin = yxLibexecDir </> "yx-this"

        binTargets =
            [ yxEnvBin
            , yxNewBin
            , yxPathBin
            , yxRemarkableBin
            , yxThisBin
            ]

        scriptNames = ("yx-" <>)
            <$> [ "apt"
                , "download"
                , "githook"
                , "jmp"
                , "mkgitignore"
                , "xpdf"
                ]

        scriptTargets = (yxLibexecDir </>) <$> scriptNames

        scriptManTargets = scriptNames <&> \baseName ->
            man1Dir </> baseName <.> "1.gz"

    want binTargets
    want scriptTargets
    want scriptManTargets

    want
        [ man1Dir </> "yx-apt.1.gz"
        , man1Dir </> "yx-download.1.gz"
        , man1Dir </> "yx-env.1.gz"
        , man1Dir </> "yx-jmp.1.gz"
        , man1Dir </> "yx-mkgitignore.1.gz"
        , man1Dir </> "yx-new.1.gz"
        , man1Dir </> "yx-path.1.gz"
        , man1Dir </> "yx-remarkable.1.gz"
        , man1Dir </> "yx-this.1.gz"
        , man1Dir </> "yx-xpdf.1.gz"
        , man1Dir </> "yx.1.gz"
        ]

    hasThisRepoChanged <- addOracle (thisGitRepo projectRoot)
    binTargets &%> \outs -> do
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

        -- Generate DB for `yx help --search`
        cmd_ "mandb --user-db /home/peter/.local/share/man"

    scriptTargets &%> \outs ->
        forM_ outs $ \out ->
            cmd_ "ln -sf"
                [ projectRoot </> "bash" </> takeFileName out
                , takeDirectory out
                ]

-- vim:ft=haskell
