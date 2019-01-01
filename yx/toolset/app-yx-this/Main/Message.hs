-- |
-- Module:      Main.Message
-- Description: Error handling that respects verbosity and colour settings.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Error handling that respects verbosity and colour settings.
module Main.Message
    ( errorMsg
    , dieWith
    )
  where

import Data.String (fromString)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (stderr)

import Data.Text (Text)
import CommandWrapper.Environment
    ( Params(Params, colour, name, subcommand, verbosity)
    )
import qualified CommandWrapper.Message as CommandWrapper (errorMsg)


errorMsg :: Params -> Text -> IO ()
errorMsg Params{colour, name, subcommand, verbosity} =
    let cmd = fromString name <> " " <> fromString subcommand
    in CommandWrapper.errorMsg cmd verbosity colour stderr

dieWith :: Params -> Int -> Text -> IO ()
dieWith params n msg = do
    errorMsg params msg
    exitWith (ExitFailure exitCode)
  where
    exitCode =
        if n <= 0 || n > 255
            then 255  -- Exit code can be only between 0-255
            else n
