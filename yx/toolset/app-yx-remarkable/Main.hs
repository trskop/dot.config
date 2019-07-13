{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
module Main (main)
  where

import Data.Function (const)
import Data.String (fromString)
import GHC.Generics (Generic)

import CommandWrapper.Prelude
    ( HaveCompletionInfo(completionInfoMode)
    , Params(Params, config)
    , completionInfoFlag
    , dieWith
    , printOptparseCompletionInfoExpression
    , stderr
    , stdout
    , subcommandParams
    )
import qualified Dhall
import qualified Turtle

import Main.Config.App (Config, readConfig)


data Mode
    = DefaultMode
    | CompletionInfo
  deriving stock (Generic, Show)

instance HaveCompletionInfo Mode where
    completionInfoMode = const CompletionInfo

main :: IO ()
main = do
    params@Params{config = configFile} <- subcommandParams
    mode <- Turtle.options description (completionInfoFlag <*> parseOptions)
    config <- readConfig
        ( \fp ->
            dieWith params stderr 1
                (fromString fp <> ": Configuration file missing.")
        )
        configFile
    realMain params config mode
  where
    description = "Better UI for existing reMarkable tools."

parseOptions :: Turtle.Parser Mode
parseOptions = pure DefaultMode

realMain :: Params -> Config -> Mode -> IO ()
realMain params _config = \case
    DefaultMode -> dieWith params stderr 125 "Not yet implemented!"
    CompletionInfo -> printOptparseCompletionInfoExpression stdout
