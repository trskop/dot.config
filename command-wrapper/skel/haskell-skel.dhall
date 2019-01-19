''
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
module Main (main)
  where

import GHC.Generics (Generic)
import System.IO (stderr)

import CommandWrapper.Prelude
    ( Params(Params, config)
    , dieWith
    , subcommandParams
    )
import qualified Dhall
import qualified Turtle


data Config = Config
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

data Mode = Mode
  deriving stock (Generic, Show)

main :: IO ()
main = do
    params@Params{config = configFile} <- subcommandParams
    mode <- Turtle.options "TODO: Describe me!" parseOptions
    config <- Dhall.inputFile Dhall.auto configFile
    realMain params config mode

parseOptions :: Turtle.Parser Mode
parseOptions = pure Mode

realMain :: Params -> Config -> Mode -> IO ()
realMain params _config = \case
    Mode -> dieWith params stderr 125 "Not yet implemented!"''
