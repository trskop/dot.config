{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module:      Main.Config
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main.Config.App
--  (
--  )
  where

import Control.Monad (unless)
import GHC.Generics (Generic)

import CommandWrapper.Internal.Dhall
    ( interpretStrictByteString
    , interpretWord
    )
import Data.HostAndPort
    ( ConnectTo
    , pattern ConnectTo
    , HostOrPortField(..)
    , connectHost
    , connectPort
    )
import qualified Data.HostAndPort as HostAndPort (interpretDhall)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Dhall
    ( Interpret(autoWith)
    , InterpretOptions(InterpretOptions, fieldModifier)
    , Type
    , auto
    , inputFile
    , maybe
    )
import System.Directory (doesFileExist)


newtype ConnectToRemarkable
  = ConnectToRemarkable (ConnectTo "remarkable" ByteString (Maybe Word))
  deriving stock (Generic, Show)

instance Dhall.Interpret ConnectToRemarkable where
    autoWith :: Dhall.InterpretOptions -> Dhall.Type ConnectToRemarkable
    autoWith Dhall.InterpretOptions{fieldModifier} = ConnectToRemarkable
        <$> interpretHostAndPort interpretStrictByteString
                (Dhall.maybe interpretWord)
      where
        interpretHostAndPort
            :: Dhall.Type ByteString
            -> Dhall.Type (Maybe Word)
            -> Dhall.Type (ConnectTo "remarkable" ByteString (Maybe Word))
        interpretHostAndPort =
            HostAndPort.interpretDhall \case
                HostField -> fieldModifier "host"
                PortField -> fieldModifier "port"

data Config = Config
    { connection :: ConnectToRemarkable
    , toolsPath :: [Text]
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

defaultConfig :: Config
defaultConfig = Config
    { connection = ConnectToRemarkable ConnectTo
        { connectHost = "10.11.99.1"
        , connectPort = Nothing -- Default SSH port.
        }
    , toolsPath = []
    }

readConfig :: (forall a. FilePath -> IO a) -> FilePath -> IO Config
readConfig die configFile = do
    configExists <- doesFileExist configFile
    unless configExists
        $ die configFile

    Dhall.inputFile Dhall.auto configFile
