{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Main
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main
--  (
--  )
  where

import Data.Functor ((<&>))
import System.Exit (die)

import CommandWrapper.Environment
    ( Params(Params, config)
    , askParams
    , parseEnvIO
    )
import qualified Data.Text.Lazy.IO as Text (putStr)
import GenBashrc.Bash (genBash)
import qualified GenBashrc.Bash as Bash (export)
import qualified Turtle


main :: IO ()
main = do
    params@Params{config} <- getEnvironment

    configExists <- Turtle.testfile (Turtle.fromString config)

    options <- parseOptions
    let configFile = if configExists then Just config else Nothing

    env params options configFile

getEnvironment :: IO Params
getEnvironment = parseEnvIO (die . show) askParams

data Mode a
    = RunHook a
    | Default a

data Config = Config

parseOptions :: IO (Mode Config)
parseOptions = Turtle.options "env"
    ( runHook <$> Turtle.switch "run-hook" 'r'
        "Execute shell hook. This is usually called when shell prompt is\
        \ redrawn."
    )
    <&> ($ Default Config)
  where
    runHook :: Bool -> Mode Config -> Mode Config
    runHook p
      | p = \case
            RunHook cfg -> RunHook cfg
            Default cfg -> RunHook cfg

      | otherwise = id

env :: Params -> Mode Config -> Maybe FilePath -> IO ()
env _params mode _ = case mode of
    Default _ ->
        die "TODO"

    RunHook _ ->
        -- TODO:
        --
        -- - Check for `YX_ENV*` environment variables.
        --
        -- - Store state stack references in `YX_ENV_STATE` environment
        --   variable.  Files should be stored in
        --   `${XDG_CACHE_HOME:$HOME/.cache}/yx-env/${hash}`.  This differs
        --   from original implementation!
        --
        -- - Apply environment delta based on state change.
        Text.putStr . genBash
            $ Bash.export "YX_ENV" (Just "1")
