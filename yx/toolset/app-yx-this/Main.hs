{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main)
  where

import Control.Applicative ((<|>))
import Data.Functor (void)
import GHC.Generics (Generic)
import System.Exit (die)

import qualified Dhall
import qualified Turtle
import CommandWrapper.Environment
    ( Params(Params, config)
    , askParams
    , parseEnvIO
    )


data Config = Config
  deriving (Generic, Show)

instance Dhall.Interpret Config

data Mode = Mode
  deriving (Generic, Show)

main :: IO ()
main = do
    params@Params{config = configFile} <- getEnvironment
    mode <- Turtle.options "TODO: Describe me!" parseOptions
    config <- Dhall.inputFile Dhall.auto configFile
    realMain params config mode

getEnvironment :: IO Params
getEnvironment = parseEnvIO (die . show) askParams

parseOptions :: Turtle.Parser Mode
parseOptions = pure Mode

realMain :: Params -> Config -> Mode -> IO ()
realMain _params _config = \case
    Mode -> die "Error: TODO: Implement me!"
