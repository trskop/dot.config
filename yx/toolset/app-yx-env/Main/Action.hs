-- |
-- Module:      Main.Action
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
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

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import qualified Data.List as List (filter, notElem)
import Data.Maybe (fromMaybe)
import System.IO
    ( Handle
    , IOMode(WriteMode)
    , stderr
    , stdout
    , withFile
    )

import CommandWrapper.Prelude
    ( Params(Params, name, subcommand)
    , dieWith
    , warningMsg
    )
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.IO as Text (putStr, writeFile)
import System.Directory (XdgDirectory(XdgCache), getXdgDirectory)
import System.FilePath ((</>))
import qualified Turtle

import Main.Config.App (Config(..), ShellScripts(..))
import qualified Main.Dhall as Dhall (hPut)
import qualified Main.Env as Env (diff)
import Main.Config.Preferences (Preferences(Preferences))
import qualified Main.Config.Preferences as Preferences
    ( KnownFile(..)
    , Preferences(..)
    , Status
    , modify
    )


-- {{{ Default, RunHook, DryRun, and Cleanup Actions --------------------------

defaultAction :: Params -> Config -> IO ()
defaultAction params _config =
    -- When implemented:
    --
    --   Start a new shell process with local environment settings in a
    --   specified directory (current directory by default).
    --
    -- TODO:
    --
    --   * Genereate temporary `bashrc` file for that specific shell with
    --     this application hooked in.  `bash --rcfile ${FILE}`
    --   * Terminate with error if we are already in "hooked in".
    dieWith params stderr 126 "TODO: Action currently not implemented."

runHookAction :: Params -> Config -> Text -> IO ()
runHookAction _params _config _pid = pure ()

dryRunAction :: Params -> Config -> FilePath -> IO ()
dryRunAction _params _config _file = pure ()

cleanupAction :: Params -> Config -> IO ()
cleanupAction _params _config = pure ()

-- }}} Default, RunHook, DryRun, and Cleanup Actions --------------------------

-- {{{ Script, Init, and SetEnvConfigStatus Actions ---------------------------

scriptAction :: Params -> Config -> IO ()
scriptAction Params{name, subcommand} Config{installScript} =
    let toolset = fromString name -- Full path would be better
        fnName = fromString name <> "_" <> fromString subcommand
        ShellScripts{bash} = installScript fnName toolset

    in Text.putStr bash

initAction :: Params -> Config -> FilePath -> IO ()
initAction params config@Config{initEnv} dir =
    Text.writeFile (dir </> mkEnvFileName params config) initEnv

setEnvConfigStatusAction
    :: Params
    -> Config
    -> Preferences.Status
    -> FilePath
    -> IO ()
setEnvConfigStatusAction params config status dir = do
    envFile <- findEnvFile (mkEnvFileName params config) (fromString dir)
    case envFile of
        Nothing ->
            warningMsg params stderr $ "No env config found in " <> gshow dir

        Just file' -> do
--          printMsg params Notice
--              $ "Setting " <> gshow file' <> " as " <> gshow status

            prefsFile <- mkPreferencesFilePath params
            Preferences.modify prefsFile $ \Preferences{knownFiles} ->
                Preferences
                    { knownFiles =
                        let file = fromString file'
                        in Preferences.KnownFile{file, status} : knownFiles
                    }

-- }}} Script, Init, and SetEnvConfigStatus Actions ---------------------------

-- {{{ Dump and Diff Actions --------------------------------------------------

dumpAction :: Params -> Config -> FilePath -> IO ()
dumpAction params _config file =
    -- TODO: Use better format than 'show' for this.  Dhall?
    dumpEnv params >>= writeFile file . show

diffAction :: Params -> Config -> FilePath -> IO ()
diffAction params _config file = do
    oldEnv <- readFile file >>= readIO
    newEnv <- dumpEnv params
    Dhall.hPut stdout (Env.diff oldEnv newEnv)

dumpEnv :: Params -> IO [(Text, Text)]
dumpEnv _params = List.filter isNotIgnoredVar <$> Turtle.env
  where
    isNotIgnoredVar (n, _) = n `List.notElem`
        [ "YX_ENV_STATE"
        , "YX_ENV_DIR"

        -- Taken from `direnv` source code, they say that it avoids
        -- segfaults in Bash.
        , "COMP_WORDBREAKS"

        , "PS1"
        , "OLDPWD", "PWD", "SHELL", "SHELLOPTS", "SHLVL", "_"
        ]

-- }}} Dump and Diff Actions --------------------------------------------------

-- {{{ Utilities --------------------------------------------------------------

-- | Provide access to controlling terminal.  This is useful in cases when
-- @stdout@/@stderr@ is redirected or used for something else then
-- communicating with the user.
--
-- TODO: Move to "CommandWrapper.Prelude" module.
withTerminal :: (Handle -> IO a) -> IO a
withTerminal = withFile "/dev/tty" WriteMode

-- | Search from specified directory upwards until env config is found.
findEnvFile
    :: String
    -- ^ Env config file name, see 'mkEnvFileName'.
    -> Turtle.FilePath
    -- ^ Directory where to start the search.  Usually current working
    -- directory.
    -> IO (Maybe FilePath)
    -- ^ Returns 'Nothing' if no such file was found.
findEnvFile envFileName' = Turtle.realpath >=> findEnvFile'
  where
    findEnvFile' dir
      | dir == Turtle.root dir = pure Nothing
      | otherwise = do
            let envFile = dir Turtle.</> envFileName
            fileExists <- Turtle.testfile envFile
            if fileExists
                then pure . Just $ Turtle.encodeString envFile
                else findEnvFile' (Turtle.parent dir)

    envFileName = Turtle.decodeString envFileName'

-- | Construct env config file name based on user preferences, and if those
-- aren't present then use 'mkDefaultEnvFileName' to construct it.
mkEnvFileName :: Params -> Config -> String
mkEnvFileName params Config{envFileName = mkConfigEnvFileName'} =
    fromMaybe (mkDefaultEnvFileName params) (mkConfigEnvFileName params)
  where
    mkConfigEnvFileName :: Params -> Maybe String
    mkConfigEnvFileName Params{name, subcommand} =
        mkConfigEnvFileName' <&> \f ->
            Text.unpack $ f (fromString name) (fromString subcommand)

-- | By default env config file is constructed as:
--
-- > .${toolset}-${subcommand}
--
-- This can be overwritten by setting 'envFileName' in configuration file. See
-- 'mkEnvFileName' for more details.
mkDefaultEnvFileName :: Params -> String
mkDefaultEnvFileName Params{name, subcommand} =
    "." <> name <> "-" <> subcommand

mkPreferencesFilePath :: Params -> IO FilePath
mkPreferencesFilePath Params{name, subcommand}  =
    getXdgDirectory XdgCache (cmdDir </> "preferences.dhall")
  where
    cmdDir = name <> "-" <> subcommand

gshow :: (IsString s, Show a) => a -> s
gshow = fromString . show

-- }}} Utilities --------------------------------------------------------------
