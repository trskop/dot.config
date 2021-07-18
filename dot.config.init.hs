#!/usr/bin/env stack
{- stack script
    --resolver lts-17.15
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
-- Copyright:   (c) 2018-2021 Peter Trško
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
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import System.Directory
    ( XdgDirectory(XdgCache, XdgConfig, XdgData)
    , createDirectoryIfMissing
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
  deriving (Binary, Eq, Hashable, NFData)

instance KnownSymbol name => Show (GitRepo name) where
    showsPrec d v = showParen (d > 10)
        $ showString "GitRepo @" . showRepoName . showString " ()"
      where
        showRepoName = shows (symbolVal v)

data GitRepoConfig (name :: Symbol) = GitRepoConfig
    { directory :: FilePath
    , url :: String
    , cloneDepth :: Maybe Word
    }
  deriving (Show)

gitRepo :: GitRepoConfig name -> GitRepo name -> Action String
gitRepo GitRepoConfig{..} GitRepo{} = do
    repoExists <- doesDirectoryExist directory
    unless repoExists
        $ cmd "git clone" (cloneDepth <&> \n -> "--clone=" <> show n) url directory
    cmd_ "git -C" [directory] "fetch --all"
    Stdout hash <- cmd "git -C" [directory] "show-ref -s origin/HEAD"
    pure hash

type instance RuleResult (GitRepo "github.com/trskop/command-wrapper") = String

mkCommandWrapperRepoConfig
    :: FilePath
    -> GitRepoConfig "github.com/trskop/command-wrapper"
mkCommandWrapperRepoConfig dotLocalDir = GitRepoConfig
    { directory = dotLocalDir </> "src" </> "github.com" </> "trskop" </> "command-wrapper"
    , url = "https://github.com/trskop/command-wrapper.git"
    , cloneDepth = Nothing
    }

type instance RuleResult (GitRepo "github.com/trskop/genbashrc") = String

mkGenbashrcRepoConfig :: FilePath -> GitRepoConfig "github.com/trskop/genbashrc"
mkGenbashrcRepoConfig dotLocalDir = GitRepoConfig
    { directory = dotLocalDir </> "src" </> "github.com" </> "trskop" </> "genbashrc"
    , url = "https://github.com/trskop/genbashrc.git"
    , cloneDepth = Nothing
    }

type instance RuleResult (GitRepo "github.com/ryanoasis/nerd-fonts") = String

mkNerdFontsRepoConfig
    :: FilePath
    -> GitRepoConfig "github.com/ryanoasis/nerd-fonts"
mkNerdFontsRepoConfig dotLocalDir = GitRepoConfig
    { directory = dotLocalDir </> "src" </> "github.com" </> "ryanoasis" </> "nerd-fonts"
    , url = "https://github.com/ryanoasis/nerd-fonts.git"
    , cloneDepth = Just 1
    }

type instance RuleResult (GitRepo "github.com/junegunn/fzf") = String

mkFzfRepoConfig :: FilePath -> GitRepoConfig "github.com/junegunn/fzf"
mkFzfRepoConfig dotLocalDir = GitRepoConfig
    { directory = dotLocalDir </> "src" </> "github.com" </> "junegunn" </> "fzf"
    , url = "https://github.com/junegunn/fzf.git"
    , cloneDepth = Nothing
    }

data Directories = Directories
    { home :: FilePath
    , srcDir :: FilePath
    , configDir :: FilePath
    , cacheDir :: FilePath
    , dataDir :: FilePath
    , stateDir :: FilePath
    }

main :: IO ()
main = do
    srcDir <- getScriptPath >>= \case
        Executable executable -> pure $ takeDirectory executable
        RunGHC script -> pure $ takeDirectory script
        Interactive -> getXdgDirectory XdgConfig ""

    stateDir <- getXdgDirectory XdgCache "dot.config"

    -- Directory locations as defined by XDG Base Directory Specification. See
    -- also `file-hierarchy(7)` section HOME DIRECTORY.
    cacheDir <- getXdgDirectory XdgCache "" -- ~/.cache by default
    configDir <- getXdgDirectory XdgConfig "" -- ~/.config by default
    dataDir <- getXdgDirectory XdgData "" -- ~/.local/share by default
    home <- getHomeDirectory

    setCurrentDirectory home
    install Directories{..} $ shakeOptions
        { shakeFiles = stateDir
        }

install :: Directories -> ShakeOptions -> IO ()
install Directories{..} opts = shakeArgs opts $ do
    let -- Locations based on XDG Base Directory Specification.
        commandWrapperDir = configDir </> "command-wrapper"
        yxDir = configDir </> "yx"
        habitDir = configDir </> "habit"
        userManDir = dataDir </> "man"

        -- TODO: Transition from "${HOME}/bin" to "${HOME}/.local/bin" and make
        -- "${HOME}/bin" a symbolic link.
        binDir = home </> "bin"

        -- See manual page `file-hierarchy(7)` section HOME DIRECTORY for more
        -- details.
        --
        -- TODO: Consider renaming some of these variables, e.g.
        -- `dotLocalLibDir → userLibDir`.
        dotLocalDir = home </> ".local"
        dotLocalLibDir = dotLocalDir </> "lib"
        dotLocalBinDir = dotLocalDir </> "bin"

        yxLibDir = dotLocalLibDir </> "yx"
        commandWrapperLibDir = dotLocalLibDir </> "command-wrapper"
        commandWrapperRepoConfig = mkCommandWrapperRepoConfig dotLocalDir
        genbashrcRepoConfig = mkGenbashrcRepoConfig dotLocalDir
        nerdFontsRepoConfig = mkNerdFontsRepoConfig dotLocalDir
        fzfRepoConfig = mkFzfRepoConfig dotLocalDir

        dejaVuSansMonoNerdFontTtf =
            dataDir </> "fonts" </> "NerdFonts"
            </> "DejaVu Sans Mono Nerd Font Complete Mono.ttf"

        deinInstallDir = cacheDir </> "dein.vim"

        nixParams = NixParams
            { version = "2.3.10"
            , signingKeyFingerprint = "B541D55301270E0BCF15CA5D8170B4726D7198DE"
            , home
            , cacheDir = stateDir </> "nix"
            }
        nixTarget = mkNixTarget nixParams

        bootstrapTargets =
            [ home </> ".bashrc"

            , home </> ".ghc/ghci.conf"
            , home </> ".haskeline"
            , home </> ".selected_editor"
            , home </> ".Xresources"
            , home </> ".psqlrc"
            , home </> ".inputrc"

            , home </> ".stack/config.yaml"
            , home </> ".stack/global-project/README.txt"
            , home </> ".stack/global-project/stack.yaml"
            , binDir </> "stack-help"

            , dotLocalBinDir </> "genbashrc"

            , commandWrapperLibDir </> "command-wrapper"
            , commandWrapperDir </> "default" <.> "dhall"

            , binDir </> "yx"
            , yxDir </> "default" <.> "dhall"
            , yxLibDir </> "yx-jmp"

            -- A lot of tools are using `fzf` including `genbashrc` when
            -- generating bashrc.
            , binDir </> "fzf"

            , nixTarget
            ]

    want $ mconcat
        [ bootstrapTargets
        , [ dejaVuSansMonoNerdFontTtf

          -- Dein is a Vim/Neovim plugin manager.
          , deinInstallDir </> "installed.lock"

          , habitDir </> "default.dhall"
--        , habitDir </> "pgpass.conf"

          , home </> ".bazelrc"

          , "man"
          ]
        ]

    "bootstrap" ~> do
        need bootstrapTargets

        -- Install `yx` toolset commands, that includes `yx this`.
        cmd_ (srcDir </> "yx" </> "toolset" </> "install")

    (home </> ".bashrc") %> \out ->
        symlink (srcDir </> "bash" </> ("dot" <> takeFileName out)) out

    -- TODO: Refactor so that "~/.ghc" is actually a symbolic link to:
    --
    --     ${XDG_CACHE_HOME:-${HOME}/.cache}/ghc
    --
    -- Path `~/.ghc/ghci.conf` should still be symbolic link to:
    --
    --     ${XDG_CONFIG_HOME:-${HOME}/.config}/ghc/ghci.conf
    --
    -- This way we will separate configuration from `ghci_history` and package
    -- databases.
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

    (dotLocalBinDir </> "genbashrc") %> \_ -> do
        _ <- genbashrcUpToDate (GitRepo ())
        let GitRepoConfig{directory} = genbashrcRepoConfig
        cmd_ "git -C" directory "pull"
        cmd_ "stack --stack-yaml" (directory </> "stack.yaml") "install"

    nerdFontsUpToDate <- addOracle (gitRepo nerdFontsRepoConfig)

    dejaVuSansMonoNerdFontTtf %> \_ -> do
        _ <- nerdFontsUpToDate (GitRepo ())
        let GitRepoConfig{directory} = nerdFontsRepoConfig
        cmd_ "git -C" directory "pull"
        cmd_ (Cwd directory) "./install.sh"

    -- {{{ CommandWrapper -----------------------------------------------------

    commandWrapperUpToDate <- addOracle (gitRepo commandWrapperRepoConfig)

    (commandWrapperLibDir </> "command-wrapper") %> \_ -> do
        _ <- commandWrapperUpToDate (GitRepo ())
        let GitRepoConfig{directory} = commandWrapperRepoConfig
        cmd_ "git -C" [directory] "pull"
        cmd_ (Cwd directory) "./install"
        cmd_ "mandb --user-db" [userManDir]

    [commandWrapperDir </> "default.dhall", commandWrapperDir </> "command-wrapper-*.dhall"]
      |%> \out -> do
        need
            [ commandWrapperLibDir </> "command-wrapper"
            , commandWrapperDir </> "library.dhall"
            ]
        let subdir = takeBaseName out `dropPrefix` "command-wrapper-"
            dir = commandWrapperDir </> subdir
            src = dir </> "constructor.dhall"

        getDirectoryFiles dir ["*"] >>= need . map (dir </>)

        -- We need to make sure that correct version of dhall command is used.
        -- Stack may give access to a different version based on resolver in
        -- this script, that is the reason for absolute path to the executable.
        cmd_
            [ commandWrapperLibDir </> "command-wrapper"
            , "--change-directory=" <> commandWrapperDir
            , "config"
            , "--dhall-freeze"
            , "--no-remote-only"
            , "--expression=" <> src
            , "--output=" <> out
            ]

    yxRules YxRulesParams
        { configDir = yxDir
        , configLib =
            [ commandWrapperDir </> "library.dhall"
            ]
        , libDir = yxLibDir
        , binDir
        , commandWrapperLibDir
        , dotLocalDir
        }

    habitRules HabitRulesParamams
        { configDir = habitDir
        , dotLocalDir
        , commandWrapperLibDir
        }

    -- }}} CommandWrapper -----------------------------------------------------

    fzfRepoUpToDate <- addOracle (gitRepo fzfRepoConfig)

    (binDir </> "fzf") %> \out -> do
        _ <- fzfRepoUpToDate (GitRepo ())
        let GitRepoConfig{directory} = fzfRepoConfig
        cmd_ (directory </> "install") "--all" "--xdg" "--no-update-rc"
        symlink (directory </> "bin" </> takeFileName out) out

    (deinInstallDir </> "installed.lock") %> \out -> do
        let installerUrl = "https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh"
            installerHash = "943163d3660423febad96fe91371807763679a683947b44b63254dc2d555094c"

            installerSh = deinInstallDir </> "installer.sh"
            installerChecksum = installerHash <> "  " <> installerSh

        Stdout installer <- cmd "curl" [installerUrl]
        writeFile' installerSh installer
        cmd_ (Stdin installerChecksum) "sha256sum -c -"
        cmd_ "bash" [installerSh, deinInstallDir]
        writeFile' out ""

    nixRules nixParams

    (srcDir </> "bazel" </> "bazelrc") %> \out -> do
        let src = srcDir </> "bazel" </> "bazelrc.dhall"
        need
            [ src
            , commandWrapperLibDir </> "command-wrapper"
            ]
        cmd_
            [ commandWrapperLibDir </> "command-wrapper"
            , "--change-directory=" <> takeDirectory src
            , "config"
            , "--dhall-text"
            , "--input=" <> src
            , "--output=" <> out
            ]

    (home </> ".bazelrc") %> \out -> do
        let src = srcDir </> "bazel" </> "bazelrc"
        need [src]
        symlink (src `dropPrefixDir` home) out

    manualPages ManualPagesParams
        { manDir = userManDir
        , manPages =
            [ (Section7, "dot.bashrc")
            , (Section7, "dot.config")
            , (Section7, "dot.config.kitty")
            , (Section7, "dot.gitconfig")
            ]
        , ..
        }

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
    , configLib :: [FilePath]
    , libDir :: FilePath
    , binDir :: FilePath
    , commandWrapperLibDir :: FilePath
    , dotLocalDir :: FilePath
    }

-- | CommandWrapper toolset `yx` is used for personal tools.
yxRules :: YxRulesParams -> Rules ()
yxRules YxRulesParams{..} = do
    [configDir </> "default.dhall", configDir </> "yx-*.dhall"] |%> \out -> do
        need [commandWrapperLibDir </> "command-wrapper"]
        let subdir = takeBaseName out `dropPrefix` "yx-"
            dir = configDir </> subdir
            src = dir </> "constructor.dhall"

        -- At the moment only "yx/default.dhall" depends on the config library.
        when (takeBaseName out == "default")
            $ need configLib

        getDirectoryFiles dir ["*"] >>= need . map (dir </>)

        -- We need to make sure that correct version of dhall command is used.
        -- Stack may give access to a different version based on resolver in
        -- this script, that is the reason for absolute path to the executable.
        cmd_
            [ commandWrapperLibDir </> "command-wrapper"
            , "--change-directory=" <> configDir
            , "config"
            , "--dhall-freeze"
            , "--no-remote-only"
            , "--expression=" <> src
            , "--output=" <> out
            ]

    (libDir </> "yx-jmp") %> \out ->
        let src = configDir </> "toolset" </> "bash" </> "yx-jmp"
        in symlink src out

    (binDir </> "yx") %> \out -> do
        need [commandWrapperLibDir </> "command-wrapper"]
        let src = commandWrapperLibDir </> "command-wrapper"
        symlink src out


data HabitRulesParamams = HabitRulesParamams
    { configDir :: FilePath
    , dotLocalDir :: FilePath
    , commandWrapperLibDir :: FilePath
    }

-- | CommandWrapper toolset `habit` is used at work. Most of the configuration
-- is not actually in the repository, only the skeleton.
habitRules :: HabitRulesParamams -> Rules ()
habitRules HabitRulesParamams{..} = do
    (configDir </> "*.dhall") %> \out -> do
        need [commandWrapperLibDir </> "command-wrapper"]
        let subdir = takeBaseName out `dropPrefix` "habit-"
            dir = configDir </> subdir
            src = dir </> "constructor.dhall"

        getDirectoryFiles dir ["*"] >>= need . map (dir </>)

        -- We need to make sure that correct version of dhall command is used.
        -- Stack may give access to a different version based on resolver in
        -- this script, that is the reason for absolute path to the executable.
        cmd_ (Stdin src) (FileStdout out)
            [ commandWrapperLibDir </> "command-wrapper"
            , "config"
            , "--dhall-freeze"
            , "--no-remote-only"
            ]

{- TODO: Currently unused, will need to be revisited.
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
-}

data NixParams = NixParams
    { home :: FilePath
    , cacheDir :: FilePath
    , version :: String
    , signingKeyFingerprint :: String
    }

mkNixTarget :: NixParams -> FilePath
mkNixTarget NixParams{cacheDir, version} =
    cacheDir </> version <> "-nix-installed.lock"

nixRules :: NixParams -> Rules ()
nixRules params@NixParams{cacheDir, signingKeyFingerprint, version} = do
    installerSh %> \out -> do
        Stdout installer <- cmd "curl" [installerUrl]
        writeFile' out installer

        Stdout installerSig <- cmd "curl" [installerSigUrl]
        writeFile' (out <.> "sig") installerSig
        gpgVerify (out <.> "sig")

    mkNixTarget params %> \out -> do
        need [installerSh]

        -- We don't want `.bashrc` file to be modified, instead we want this to
        -- be handled by `genbashrc`.
        cmd_ (AddEnv "NIX_INSTALLER_NO_MODIFY_PROFILE" "1") "sh" [installerSh]
            "--no-daemon"
        writeFile' out ""
  where
    baseUrl = "https://releases.nixos.org/nix"
    installerUrl = baseUrl <> "/nix-" <> version <> "/install"
    installerSigUrl = baseUrl <> "/nix-" <> version <> "/install.asc"

    installerSh = cacheDir </> version <> "-installer"

    gpgVerify sig = do
        Stdout distro <- cmd "lsb_release --short --id"
        let (gpgCmd, recvKeysOptions) = case takeWhile (/= '\n') distro of
                "Ubuntu" ->
                    ("gpg2", ["--keyserver", "hkp://keyserver.ubuntu.com"])

                "Debian" -> do
                    ("gpg", [signingKeyFingerprint])

                _ ->
                    error (show distro <> ": Distribution not supported!")

        cmd_ gpgCmd "--recv-keys" recvKeysOptions [signingKeyFingerprint]
        cmd_ gpgCmd ["--verify", sig]

data ManualSection
    = Section1
    -- ^ Executable programs or shell commands.
    | Section5
    -- ^ File formats and conventions.
    | Section7
    -- ^ Miscellaneous (including macro packages and conventions).

renderManualSection :: ManualSection -> String
renderManualSection = \case
    Section1 -> "1"
    Section5 -> "5"
    Section7 -> "7"

manualPageToPath :: FilePath -> ManualSection -> String -> FilePath
manualPageToPath dir section page =
    dir </> "man" <> section' </> page <.> section'
  where
    section' = renderManualSection section

data ManualPagesParams = ManualPagesParams
    { srcDir :: FilePath
    , manDir :: FilePath
    , manPages :: [(ManualSection, String)]
    }

manualPages :: ManualPagesParams -> Rules ()
manualPages ManualPagesParams{..} = do
    -- Pandoc may not be installed yet so we don't want to install these from
    -- the get go.
    "man" ~> do
        need $ manPages <&> \(section, page) ->
            manualPageToPath manDir section page <.> "gz"

    (sections <&> \s -> manualPageToPath manDir s "*" <.> "gz") |%> \out -> do
        let tempOut = dropExtension out
            src = srcDir </> "man" </> takeBaseName out <.> "md"
            dst = takeDirectory out

        need [src]

        liftIO (createDirectoryIfMissing True dst)

        cmd_ "pandoc --standalone --to=man" ["--output=" <> tempOut, src]
        cmd_ "gzip --force -9" [tempOut]
        cmd_ "mandb --user-db " [manDir]
  where
    sections = [Section1, Section5, Section7]

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
