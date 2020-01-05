{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Main.Config.App
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018-2020 Peter Trško
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
    , parseConfig
    )
  where

import Control.Monad (when)
import Data.Maybe (Maybe)
import GHC.Generics (Generic)
import System.IO (IO)

import Data.Text (Text)
import qualified Data.Text as Text (null)
import Dhall (FromDhall, ToDhall)
import qualified Dhall (auto, input)


data ShellScripts a = ShellScripts
    { bash :: a
    , fish :: a
    , tcsh :: a
    , zsh :: a
    }
  deriving stock (Generic)
  deriving anyclass (FromDhall, ToDhall)

data Config = Config
    { installScript :: Text -> Text -> ShellScripts Text
    , envFileName :: Maybe (Text -> Text -> Text)
    , initEnv :: Text
    }
  deriving stock (Generic)
  deriving anyclass (FromDhall)

parseConfig :: (forall a. IO a) -> Text -> IO Config
parseConfig die configExpr = do
    when (Text.null configExpr) die
    Dhall.input Dhall.auto configExpr
