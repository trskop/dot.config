''
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
module Main (main)
  where

import Data.Function (const)
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


data Config = Config
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

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
    config <- Dhall.inputFile Dhall.auto configFile
    realMain params config mode
  where
    description = "TODO: Hereby I promise to describe this subcommand one day."

parseOptions :: Turtle.Parser Mode
parseOptions = pure DefaultMode

realMain :: Params -> Config -> Mode -> IO ()
realMain params _config = \case
    DefaultMode -> dieWith params stderr 125 "Not yet implemented!"
    CompletionInfo -> printOptparseCompletionInfoExpression stdout''