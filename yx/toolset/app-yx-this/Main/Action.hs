-- |
-- Module:      Main.Action
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module Main.Action
--  (
--  )
  where

--import Control.Applicative (empty)
import Control.Monad (when)
import GHC.Generics (Generic)
import qualified System.IO as IO (stderr, stdout)
import System.Process (callProcess)

import CommandWrapper.Prelude
    ( Params(Params, colour, verbosity)
    , shouldUseColours
    , dieWith
    )
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member, null)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Verbosity as Verbosity (Verbosity(..))
import Turtle hiding (die)
import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)

import Main.Config.App
    ( Config(..)
    , Defaults(..)
    , NixConfig(..)
    , SystemConfig(..)
    , UpdateWhat(..)
    )



-- {{{ Update Action ----------------------------------------------------------

updateAction :: Params -> Config -> Set UpdateWhat -> IO ()
updateAction params config what' = do
    when (Set.member UpdateSystem what) $ do
        setTimezone timezone
        updateSystem

    when (Set.member InstallPackages what)
        $ installPackages purgePackages (bootstrapPackages <> packages)

    when (Set.member UpdateUserEnvironment what)
        $ updateUserEnvironment params nix
  where
    Config
        { defaults = Defaults
            { update = updateWhatDefault
            }

        , system = SystemConfig
            { bootstrapPackages
            , purgePackages
            , packages
            , timezone
            }

        , nix
        } = config

    what
      | Set.null what', null updateWhatDefault =
            Set.fromList [minBound..maxBound]

      | Set.null what' =
            Set.fromList updateWhatDefault

      | otherwise =
            what'

setTimezone :: Text -> IO ()
setTimezone expectedTimezone = do
    currentTimezone <- single
        . inproc "sed" ["-rn", "s/^ *Time zone: *([^ ]+) .*$/\\1/p"]
        $ inproc "timedatectl" ["status"] empty

    -- TODO: Print message informing about this being performed.
    when (lineToText currentTimezone /= expectedTimezone)
        $ procs "sudo" ["timedatectl", "set-timezone", expectedTimezone] empty

updateSystem :: IO ()
updateSystem = do
    -- TODO: Respect verbosity.
    echo ": sudo apt update"
    callProcess "sudo" ["apt", "update"]

    echo ": sudo apt dist-upgrade --download-only --yes"
    callProcess "sudo" ["apt", "dist-upgrade", "--download-only", "--yes"]

    echo ": sudo apt dist-upgrade"
    callProcess "sudo" ["apt", "dist-upgrade"]

installPackages :: [Text] -> [Text] -> IO ()
installPackages purge install = do
    printf (": sudo apt purge " % w % "\n") purge
    callProcess "sudo" (["apt", "purge"] <> fmap Text.unpack purge)

    printf (": sudo apt install " % w % "\n") install
    callProcess "sudo" (["apt", "install"] <> fmap Text.unpack install)

updateUserEnvironment :: Params -> NixConfig -> IO ()
updateUserEnvironment Params{colour, verbosity} NixConfig{packages} = do
    useColours <- liftIO (shouldUseColours IO.stdout colour)
    let shakeOptions =
            ( if useColours
                then ["--colour"]
                else ["--no-colour"]
            )
            <>  ( case verbosity of
                    Verbosity.Silent -> ["--silent"]
                    Verbosity.Normal -> []
                    Verbosity.Verbose -> ["--verbose"]
                    Verbosity.Annoying -> ["--debug"]
                )

    dotConfigInit <- liftIO (getXdgDirectory XdgConfig "dot.config.init.hs")
    printf (": " % w % " " % w % "\n") dotConfigInit shakeOptions
    callProcess dotConfigInit shakeOptions

    yxInstall <- liftIO (getXdgDirectory XdgConfig "yx/toolset/install")
    printf (": " % w % " " % w % "\n") yxInstall shakeOptions
    callProcess yxInstall shakeOptions

    let nixEnv = "nix-env"

        nixEnvOptions =
            [ "--install"
            , "--prebuilt-only"
            , "--preserve-installed"
            ] <> fmap Text.unpack packages

    unless (null packages) $ do
        printf (": " % w % " " % w % "\n") nixEnv nixEnvOptions
        callProcess nixEnv nixEnvOptions

-- }}} Update Action ----------------------------------------------------------

-- {{{ Edit Action ------------------------------------------------------------

data EditWhat
    = ThisConfigFile
    | SshConfig
  deriving stock (Eq, Generic, Ord, Show)

editAction :: Params -> Config -> EditWhat -> IO ()
editAction params _ _ =
    dieWith params IO.stderr 126 "'--edit': TODO: Implement me!"

-- }}} Edit Action ------------------------------------------------------------

-- {{{ Default Action ---------------------------------------------------------

defaultAction :: Params -> Config -> IO ()
defaultAction params _ =
    dieWith params IO.stderr 126 "TODO: Implement me!"

-- }}} Default Action ---------------------------------------------------------
