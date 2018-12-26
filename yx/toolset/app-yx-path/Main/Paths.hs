{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Main.Paths
-- Description: Generate Dhall configuration file with XDG Directories.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Generate Dhall configuration file with XDG Directories.
module Main.Paths
    ( Paths(..)
    , Xdg(..)
    , XdgUserDirs(..)
    , Toolset(..)
    , CommandWrapper(..)
    , mk
    )
  where

import Control.Applicative (liftA2)
import qualified Data.Char as Char (toUpper)
import qualified Data.List as List (takeWhile)
import Data.String (IsString, fromString)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

import CommandWrapper.Environment (Params(Params, exePath, name))
import Data.Text (Text)
import qualified Dhall (Inject)
import System.Directory
    ( XdgDirectory(XdgCache, XdgConfig, XdgData)
    , getXdgDirectory
    )
import System.Directory.ProjectRoot (getProjectRootCurrent)
import System.FilePath ((</>))
import System.Process (readProcess)


-- TODO:
-- data System = System
--     { os :: OperatingSystem
--     , cpuInfo :: CpuInfo
--     -- ^ Number of CPUs, physical cores, virtual cores, ...
--     , ramSize :: Natural
--     }

data Paths = Paths
    { xdg :: Xdg
    , projectRoot :: Maybe Text
    , commandWrapper :: CommandWrapper
    , editor :: Text

-- TODO:
--  , pager :: Text
    }
  deriving (Generic, Show)

instance Dhall.Inject Paths

data Xdg = Xdg
    { userDirs :: XdgUserDirs
    , configDir :: Text
    , cacheDir :: Text
    , dataDir :: Text
    }
  deriving (Generic, Show)

instance Dhall.Inject Xdg

data XdgUserDirs = XdgUserDirs
    { desktop :: Text
    , download :: Text
    , templates :: Text
    , publicShare :: Text
    , documents :: Text
    , music :: Text
    , pictures :: Text
    , videos :: Text
    }
  deriving (Generic, Show)

instance Dhall.Inject XdgUserDirs

data XdgUserDir
    = Desktop
    | Download
    | Templates
    | PublicShare
    | Documents
    | Music
    | Pictures
    | Videos
  deriving Show

data Toolset = Toolset
    { configDir :: Text
    , configFile :: Text
--  , exeFile :: Text
--  , libDir :: Text
    }
  deriving (Generic, Show)

instance Dhall.Inject Toolset

data CommandWrapper = CommandWrapper
    { configDir :: Text
    , configFile :: Text
    , exeFile :: Text
--  , libDir :: Text
    , toolset :: Toolset
    }
  deriving (Generic, Show)

instance Dhall.Inject CommandWrapper

mk :: Params -> IO Paths
mk Params{name, exePath} = do
    cacheDir <- fromString <$> getXdgDirectory XdgCache ""
    configDir <- fromString <$> getXdgDirectory XdgConfig ""
    dataDir <- fromString <$> getXdgDirectory XdgData ""

    toolset <- Toolset
        <$> (fromString <$> getXdgDirectory XdgConfig name)
        <*> (fromString <$> getXdgDirectory XdgConfig (name </> "default.dhall"))
    commandWrapper <- CommandWrapper
        <$> (fromString <$> getXdgDirectory XdgConfig "command-wrapper")
        <*>  (fromString <$> getXdgDirectory XdgConfig ("command-wrapper" </> "default.dhall"))
        <*> pure (fromString exePath)
        <*> pure toolset

    desktop <- execXdgUserDir Desktop
    download <- execXdgUserDir Download
    templates <- execXdgUserDir Templates
    publicShare <- execXdgUserDir PublicShare
    documents <- execXdgUserDir Documents
    music <- execXdgUserDir Music
    pictures <- execXdgUserDir Pictures
    videos <- execXdgUserDir Videos

    projectRoot <- fmap fromString <$> getProjectRootCurrent

    let -- This default is used in all cases, however, in case of dumb terminal
        -- it should probably be 'ex'.
        defaultEditor = "vi"

    editor <- maybe defaultEditor fromString <$> do
        let lookupEditor = lookupEnv "EDITOR"

            lookupVisualThenEditor =
                lookupEnv "VISUAL" >>= maybe lookupEditor (pure . Just)

            checkIfDumbTerminal = liftA2 (||) null (== "dumb")

        -- https://unix.stackexchange.com/questions/4859/visual-vs-editor-what-s-the-difference
        isDumbTerminal <- maybe True checkIfDumbTerminal <$> lookupEnv "TERM"
        if isDumbTerminal
            then lookupEditor
            else lookupVisualThenEditor

        -- TODO:
        --
        -- - Support Debian/Ubuntu select-editor and sensible-editor
        --   functionality.
        -- - It may be useful to check that the command exists on the system
        --   and resolve it to a full path.

    pure Paths
        { xdg = Xdg
            { configDir
            , cacheDir
            , dataDir
            , userDirs = XdgUserDirs
                { desktop
                , download
                , templates
                , publicShare
                , documents
                , music
                , pictures
                , videos
                }
            }
        , projectRoot
        , commandWrapper
        , editor
        }

execXdgUserDir :: IsString s => XdgUserDir -> IO s
execXdgUserDir = execXdgUserDir' . xdgUserDirsToArgument
  where
    xdgUserDirsToArgument = fmap Char.toUpper . show

    execXdgUserDir' name =
        toFilePathText <$> readProcess "xdg-user-dir" [name] ""

    toFilePathText = fromString . List.takeWhile isNotCrLf

    isNotCrLf ch = ch /= '\r' && ch /= '\n'
