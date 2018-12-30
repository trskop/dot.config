{-# LANGUAGE OverloadedLists #-}
-- |
-- Module:      Main.Config.App
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main.Config.App
    (
    -- * Application Config
      Config(..)
    , ShellScripts(..)
    , readConfig
    )
  where

import Control.Monad (unless)
import GHC.Generics (Generic)

import Data.Text (Text)
import qualified Dhall (Inject, Interpret, auto, inputFile)
import qualified Turtle


data ShellScripts a = ShellScripts
    { bash :: a
    , fish :: a
    , tcsh :: a
    , zsh :: a
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

data Config = Config
    { installScript :: Text -> Text -> ShellScripts Text
    , envFileName :: Maybe (Text -> Text -> Text)
    , initEnv :: Text
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Interpret)

readConfig :: (forall a. FilePath -> IO a) -> FilePath -> IO Config
readConfig die configFile = do
    configExists <- Turtle.testfile (Turtle.fromString configFile)
    unless configExists
        $ die configFile

    Dhall.inputFile Dhall.auto configFile
