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
import Data.Proxy (Proxy(..))
import Data.String (fromString)
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
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))


newtype ConnectToRemarkableViaSsh
    = ConnectToRemarkableViaSsh
        (ConnectTo "remarkable-ssh" ByteString (Maybe Word))
  deriving stock (Generic, Show)

instance Dhall.Interpret ConnectToRemarkableViaSsh where
    autoWith :: Dhall.InterpretOptions -> Dhall.Type ConnectToRemarkableViaSsh
    autoWith opts = ConnectToRemarkableViaSsh
        <$> interpretHostAndPort (Proxy @"remarkable-ssh") opts
                interpretStrictByteString (Dhall.maybe interpretWord)

newtype ConnectToRemarkableViaWebUi
    = ConnectToRemarkableViaWebUi
        (ConnectTo "remarkable-web-ui" ByteString (Maybe Word))
  deriving stock (Generic, Show)

instance Dhall.Interpret ConnectToRemarkableViaWebUi where
    autoWith
        :: Dhall.InterpretOptions
        -> Dhall.Type ConnectToRemarkableViaWebUi
    autoWith opts = ConnectToRemarkableViaWebUi
        <$> interpretHostAndPort (Proxy @"remarkable-web-ui") opts
                interpretStrictByteString (Dhall.maybe interpretWord)

interpretHostAndPort
    :: forall (tag :: k) host port
    .  Proxy tag
    -> Dhall.InterpretOptions
    -> Dhall.Type host
    -> Dhall.Type port
    -> Dhall.Type (ConnectTo tag host port)
interpretHostAndPort Proxy Dhall.InterpretOptions{fieldModifier} =
    HostAndPort.interpretDhall \case
        HostField -> fieldModifier "host"
        PortField -> fieldModifier "port"

data Config = Config
    { ssh :: ConnectToRemarkableViaSsh
    , webUi :: ConnectToRemarkableViaWebUi
    , toolsPath :: [Text]
    , syncDir :: Text
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

defaultConfig :: IO Config
defaultConfig = do
    home <- getHomeDirectory

    pure Config
        { ssh = ConnectToRemarkableViaSsh ConnectTo
            { connectHost = "10.11.99.1"
            , connectPort = Nothing -- Default SSH port.
            }
        , webUi = ConnectToRemarkableViaWebUi ConnectTo
            { connectHost = "10.11.99.1"
            , connectPort = Nothing -- Default 9000.
            }
        , toolsPath = []
        , syncDir = fromString (home </> "Documents" </> "reMarkable")
        }

readConfig :: (forall a. FilePath -> IO a) -> FilePath -> IO Config
readConfig die configFile = do
    configExists <- doesFileExist configFile
    unless configExists
        $ die configFile

    Dhall.inputFile Dhall.auto configFile
