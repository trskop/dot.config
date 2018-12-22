-- |
-- Module:      Main
-- Description: Generate Dhall configuration file with XDG Directories.
-- Copyright:   (c) 2018 Peter Trško
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

import CommandWrapper.Environment
    ( Params(Params, config)
    , askParams
    , parseEnvIO
    )
import Data.Monoid.Endo (E)
import Data.Text (Text)
import qualified Data.Text as Text (strip, {-unlines,-} unwords)
import qualified Data.Text.IO as Text (getContents{-, putStr-})
import System.Exit (die{-, exitSuccess-})
import qualified Turtle

import Main.Dhall (dhall)
import qualified Main.Dhall as Dhall (Options(..), Output(..))
import qualified Main.Paths as Paths (mk)


main :: IO ()
main = do
    params@Params{config} <- getEnvironment

    configExists <- Turtle.testfile (Turtle.fromString config)

    (dhallOptions, possibleExpressionText) <- parseOptions
    paths <- Paths.mk params
    let configFile = if configExists then Just config else Nothing
    dhall dhallOptions paths configFile =<< case possibleExpressionText of
        "" -> pure "data : {paths : Paths, config : Config}"
        "-" -> Text.getContents
        _ -> pure possibleExpressionText

getEnvironment :: IO Params
getEnvironment = parseEnvIO (die . show) askParams

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

parseOptions :: IO (Dhall.Options, Text)
parseOptions = Turtle.options "Paths"
    ( go
        <$> plainOption
        <*> typeOption
        <*> many (Turtle.argText "EXPRESSION" "Dhall expression.")
    )
  where
    plainOption, typeOption :: Turtle.Parser (E Dhall.Output)
    plainOutput, typeOutput :: Bool -> E Dhall.Output

    plainOption =
        plainOutput <$> Turtle.switch "plain" 'p'
            "Plain output, Dhall expression must result in one of: Text,\
            \ Natural, or Integer."

    plainOutput p
      | p = const Dhall.Plain
      | otherwise = id

    typeOption =
        typeOutput <$> Turtle.switch "type" 't'
            "Print Dhall type instead of a value"

    typeOutput p
      | p = const Dhall.DhallType
      | otherwise = id

    go :: E Dhall.Output -> E Dhall.Output -> [Text] -> (Dhall.Options, Text)
    go f g as =
        ( Dhall.Options
            { Dhall.output = f (g Dhall.DhallExpression)
            }
        , Text.strip (Text.unwords as)
        )

-- TODO:
--
-- - Find a better name for this command.  Its scope is broader then just
--   returning interesting paths
-- - System information (OS, distribution, architecture, number of CPUs,
--   RAM size, ...).  See also TODOs in `Main.Paths` module.
