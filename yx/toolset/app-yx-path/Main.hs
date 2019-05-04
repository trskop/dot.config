-- |
-- Module:      Main
-- Description: Generate Dhall configuration file with XDG Directories.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Generate Dhall configuration file with XDG Directories.
module Main (main)
  where

import Control.Applicative (many)
--import Data.String (fromString)

import CommandWrapper.Prelude
    ( HaveCompletionInfo(completionInfoMode)
    , Params(Params, config)
    , completionInfoFlag
    , printOptparseCompletionInfoExpression
    , stdout
    , subcommandParams
    )
import Data.Monoid.Endo (E)
import Data.Text (Text)
import qualified Data.Text as Text (strip, {-unlines,-} unwords)
import qualified Data.Text.IO as Text (getContents{-, putStr-})
import qualified Turtle

import Main.Dhall (dhall)
import qualified Main.Dhall as Dhall (Options(..), Output(..))
import qualified Main.Paths as Paths (mk)


data Mode
    = DefaultMode Dhall.Options Text
    | CompletionInfo

instance HaveCompletionInfo Mode where
    completionInfoMode = const CompletionInfo

main :: IO ()
main = do
    params <- subcommandParams
    parseOptions >>= \case
        DefaultMode dhallOptions possibleExpression ->
            defaultAction params dhallOptions possibleExpression

        CompletionInfo ->
            printOptparseCompletionInfoExpression stdout
  where
    defaultAction params@Params{config} dhallOptions possibleExpression = do
        configExists <- Turtle.testfile (Turtle.fromString config)
        paths <- Paths.mk params
        let configFile = if configExists then Just config else Nothing
        dhall dhallOptions paths configFile =<< case possibleExpression of
            "" -> pure "data : {paths : Paths, config : Config}"
            "-" -> Text.getContents
            _ -> pure possibleExpression


--printHelp :: Params -> IO ()
--printHelp Params{name} = Text.putStr . Text.unlines $ fmap Text.unwords
--    [ ["Usage:"]
--    , []
--    , [" ", fromString name, "path", options, "[EXPRESSION]"]
--    , [" ", fromString name, "path", options, "-- [EXPRESSION [...]]"]
--    , [" ", fromString name, "path", "{-h|--help}"]
--    ]
--  where
--    options = "[--plain|--type]"

parseOptions :: IO Mode
parseOptions = Turtle.options "Paths" $ completionInfoFlag
    <*> ( go
            <$> plainOption
            <*> typeOption
            <*> many (Turtle.argText "EXPRESSION" "Dhall expression.")
        )
  where
    plainOption, typeOption :: Turtle.Parser (E Dhall.Output)
    plainOutput, typeOutput :: Bool -> E Dhall.Output

    plainOption =
        plainOutput <$> Turtle.switch "plain" 'p'
            "Plain output, final Dhall expression must result in one of: Text,\
            \ Natural, or Integer"

    plainOutput p
      | p = const Dhall.Plain
      | otherwise = id

    typeOption =
        typeOutput <$> Turtle.switch "type" 't'
            "Print type of final Dhall expression instead of its value"

    typeOutput p
      | p = const Dhall.DhallType
      | otherwise = id

    go :: E Dhall.Output -> E Dhall.Output -> [Text] -> Mode
    go f g as = DefaultMode
        Dhall.Options
            { Dhall.output = f (g Dhall.DhallExpression)
            }
        (Text.strip (Text.unwords as))

-- TODO:
--
-- - Find a better name for this command.  Its scope is broader then just
--   returning interesting paths
--
-- - System information (OS, distribution, architecture, number of CPUs,
--   RAM size, ...).  See also TODOs in `Main.Paths` module.
--
-- - Having 'projectRoot' entry is nice, but there are other information that
--   would be useful to have.  Like what VCS is used, what build system it
--   uses, etc.
--
-- - In case of `--plain` we may want to be able to provide delimiter.
--   Especially for cases when we are returning lists.
