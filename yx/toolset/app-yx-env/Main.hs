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
module Main (main)
  where

import Control.Exception (bracket)
import Control.Monad ((>=>), join, when, unless)
import Data.Foldable (asum, fold, for_)
import Data.Functor ((<&>))
import qualified Data.List as List (find)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Traversable (for)
import System.Exit (die, exitFailure, exitSuccess)
import System.IO
    ( Handle
    , IOMode(WriteMode)
    , hClose
    , hPutStr
    , hPutStrLn
    , openTempFile
    , withFile
    )

import CommandWrapper.Environment
    ( Params(Params, colour, config, name, subcommand, verbosity)
    , askParams
    , parseEnvIO
    )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import Data.Output.Colour (useColoursWhen, terminalSupportsColours)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.IO as Text (hPutStr, putStr, writeFile)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text (null, toStrict)
import qualified Data.Text.Lazy.IO as Lazy.Text (putStr)
import Data.Verbosity (Verbosity(..))
import GenBashrc.Bash (genBash)
import qualified System.Console.ANSI as AnsiTerminal
    ( Color(Red, White, Yellow)
    , ColorIntensity(Vivid)
    , ConsoleLayer(Foreground)
    , SGR(Reset, SetColor)
    , setSGRCode
    )
import System.Console.Terminfo (setupTermFromEnv)
import System.Directory (XdgDirectory(XdgCache), getXdgDirectory)
import System.FilePath ((</>), takeDirectory)
import qualified Turtle

import Main.Config
    ( Config(..)
    , Env
    , ShellScripts(..)
    , readConfig
    , readEnvConfig
    )
import Main.Config.Preferences (Preferences(Preferences))
import qualified Main.Config.Preferences as Preferences
    ( KnownFile(..)
    , Preferences(..)
    , Status(..)
    , modify
    , read
    )
import qualified Main.Bash as Bash
    ( applyEnv
    , reverseOperations
    , setState
    , unsetState
    )
import Main.Env (SetOrUnsetEnvVar)
import qualified Main.Env as Env (apply)
import Main.State (File(..), State(..), emptyState, readState, writeState)


main :: IO ()
main = do
    params@Params{config = configFile} <- getEnvironment
    config <- readConfig (dieMissingConfig params) configFile
    mode <- parseOptions config
    env params mode

dieMissingConfig :: Params -> FilePath -> IO a
dieMissingConfig params fp = do
    printMsg params Error
        $ "Missing configuration file " <> gshow fp
    exitFailure

getEnvironment :: IO Params
getEnvironment = parseEnvIO (die . show) askParams

data Mode a
    -- TODO: Command to show preferences.
    = RunHook Text a
    | Script a
    | DryRun FilePath a
    | Init FilePath a
    | Cleanup a
    | SetEnvConfigStatus Preferences.Status FilePath a
    | Default a
  deriving stock (Show)

switchMode :: (forall b. b -> Mode b) -> Mode cfg -> Mode cfg
switchMode f = \case
    RunHook _ cfg -> f cfg
    Script cfg -> f cfg
    DryRun _ cfg -> f cfg
    Init _ cfg -> f cfg
    Cleanup cfg -> f cfg
    SetEnvConfigStatus _ _ cfg -> f cfg
    Default cfg -> f cfg

switchMode1 :: (forall b. a -> b -> Mode b) -> a -> Mode cfg -> Mode cfg
switchMode1 f a = switchMode (f a)

-- TODO: Switch to pure @optparse-applicative@ to gain full control over the
-- TUI.
parseOptions :: Config -> IO (Mode Config)
parseOptions config = Turtle.options "env" options <&> ($ Default config)
  where
    options = asum
        [ runHookMode <$> Turtle.optText "run-hook" 'r'
            "Execute shell hook.  This is usually called when shell prompt is\
            \ redrawn.  See also '--script' option."

        , hookScriptMode <$> Turtle.switch "script" 's'
            "Script to be included in shell configuration, e.g. '.bashrc'."

        , initMode <$> Turtle.switch "init" 'i'
            "Create env config in the current directory."

        , cleanupMode <$> Turtle.switch "cleanup" 'c' "Perform cleanup"

        , dryRunMode <$> Turtle.optText "dry-run" 'u'
            "Print out what would happen if we cded into specified directory."

        , setStatusMode Preferences.Allowed <$> Turtle.optText "allow" 'a'
            "Allow specified env config to be used to modify environment."

        , setStatusMode Preferences.Ignored <$> Turtle.optText "ignore" 'g'
            "Ignore specified env config instead of using it to modify\
            \ environment."

        , pure id
        ]

    runHookMode :: Text -> Mode Config -> Mode Config
    runHookMode = switchMode1 RunHook

    hookScriptMode :: Bool -> Mode Config -> Mode Config
    hookScriptMode p
      | p = switchMode Script
      | otherwise = id

    initMode :: Bool -> Mode Config -> Mode Config
    initMode p
      | p = switchMode (Init ".")
      | otherwise = id

    dryRunMode :: Text -> Mode Config -> Mode Config
    dryRunMode = switchMode1 DryRun . Text.unpack

    cleanupMode :: Bool -> Mode Config -> Mode Config
    cleanupMode p
      | p = switchMode Cleanup
      | otherwise = id

    setStatusMode :: Preferences.Status -> Text -> Mode Config -> Mode Config
    setStatusMode s = switchMode1 (SetEnvConfigStatus s) . Text.unpack

env :: Params -> Mode Config -> IO ()
env params@Params{name, subcommand} = \case
    Default _ ->
        -- When implemented:
        --
        --   Start a new shell process with local environment settings in a
        --   specified directory (current directory by default).
        --
        -- TODO:
        --
        --   * Genereate temporary `bashrc` file for that specific shell with
        --     this application hooked in.
        --   * Terminate with error if we are already in "hooked in".
        die "TODO"

    DryRun dir config -> do
        possiblyEnvFile <- findEnvFile (mkEnvFileName params config)
            (Turtle.decodeString dir)

        for_ possiblyEnvFile $ \envFile ->
            readEnvConfig envFile >>= \case
                Nothing -> pure ()
                Just (_hash, envCfg) -> do
                    envVars <- HashMap.fromList <$> Turtle.env
                    let envDir = fromString (takeDirectory envFile)
                        (envOps, applyEnv) =
                            Bash.applyEnv envDir envVars envCfg
                    Lazy.Text.putStr applyEnv
                    Lazy.Text.putStr "===\n"
                    Lazy.Text.putStr (genBash $ Bash.reverseOperations envOps)

    Script Config{installScript} ->
        let toolset = fromString name -- Full path would be better
            fnName = fromString name <> "_" <> fromString subcommand
            ShellScripts{bash} = installScript fnName toolset

        in  Text.putStr bash

    RunHook shellPid config -> do
        let envFileName = mkEnvFileName params config
        envFile' <- Turtle.pwd >>= findEnvFile envFileName

        -- TODO: We may want to move most of this logic into 'findEnvFile'.
        envFile <- fmap join . for envFile' $ \file -> do
            Preferences{knownFiles} <-
                mkPreferencesFilePath params >>= Preferences.read

            let lookupFile fp =
                    List.find ((== fp) . Preferences.file) knownFiles
            case Preferences.status <$> lookupFile (fromString file) of
                Just Preferences.Allowed ->
                    pure envFile'

                -- We need to skip ignored file to not change existing
                -- environment.  Otherwise we would enter a directory with a
                -- disallowed env config and unload current environment, if one
                -- is loaded.
                Just Preferences.Ignored ->
                    findEnvFile envFileName
                        -- TODO: Should we make sure that
                        -- 'takeDirectory . takeDirectory /= takeDirectory'?
                        . fromString . takeDirectory $ takeDirectory file

                Nothing -> do
                    let baseCmd =
                            fromString name <> " " <> fromString subcommand

                    printMsg params Warning $ fold
                        [ "Ignoring ", gshow file, ".\n\n"
                        , "To allow it run:\n\n"
                        , "    ", baseCmd, " --allow .\n\n"
                        , "To ignore it run:\n\n"
                        , "    ", baseCmd, " --ignore ."
                        ]
                    exitSuccess

        envVars <- HashMap.fromList <$> Turtle.env
        let stateFile = HashMap.lookup stateEnvVar envVars
        -- TODO: Handle parse, type, and other read state errors correctly.
        state <- maybe (pure emptyState) (readState . Text.unpack) stateFile

        possiblyParsedConfig <-
            maybe (pure Nothing) (\fp -> fmap (fp, ) <$> readEnvConfig fp)
                envFile

        let possiblyEnvFile = do
                possiblyParsedConfig <&> \(file, (hash, _)) -> File
                    { file = fromString file
                    , hash
                    }

        if  | isStateStale state possiblyEnvFile -> do
                leaveEnv params state envVars
                    >>= enterEnv stateEnvVar params possiblyParsedConfig shellPid

                -- Function `enterEnv` created a new state file. It is safe to
                -- delete the old one.
                for_ stateFile $ Turtle.rm . Turtle.fromText

            | shellChanged state shellPid -> do
                -- Clone state file so that the two can evolve independently.
                writeStateFile stateEnvVar params state{shellPid}

            | otherwise -> pure ()

    Init dir config@Config{initEnv} ->
        Text.writeFile (dir </> mkEnvFileName params config) initEnv

    -- This command is used in following cases:
    --
    -- - Manual cleanup.
    -- - Cleanup on shell termination.
    Cleanup _config -> do
        possiblyStateFile <- lookup stateEnvVar <$> Turtle.env
        for_ possiblyStateFile $ \stateFile -> do
            -- TODO: Restore the previous environment.
            Lazy.Text.putStr (Bash.unsetState stateEnvVar)
            Turtle.rm (Turtle.fromText stateFile)

    SetEnvConfigStatus status dir config -> do
        envFile <- findEnvFile (mkEnvFileName params config) (fromString dir)
        case envFile of
            Nothing ->
                printMsg params Warning
                    $ "No env config found in " <> gshow dir

            Just file' -> do
                printMsg params Notice
                    $ "Setting " <> gshow file' <> " as " <> gshow status

                prefsFile <- mkPreferencesFilePath params
                Preferences.modify prefsFile $ \Preferences{knownFiles} ->
                    Preferences
                        { knownFiles =
                            let file = fromString file'
                            in Preferences.KnownFile{file, status} : knownFiles
                        }
  where
    stateEnvVar = "YX_ENV_STATE" :: Text

isStateStale :: State -> Maybe File -> Bool
isStateStale State{config} currentConfig = config /= currentConfig

shellChanged :: State -> Text -> Bool
shellChanged State{shellPid} currentShellPid = shellPid /= currentShellPid

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

-- | By default env config file is constructed as:
--
-- > .${toolset}-${subcommand}
--
-- This can be overwritten by setting 'envFileName' in configuration file. See
-- 'mkEnvFileName' for more details.
mkDefaultEnvFileName :: Params -> String
mkDefaultEnvFileName Params{name, subcommand} =
    "." <> name <> "-" <> subcommand

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

mkPreferencesFilePath :: Params -> IO FilePath
mkPreferencesFilePath Params{name, subcommand}  =
    getXdgDirectory XdgCache (cmdDir </> "preferences.dhall")
  where
    cmdDir = name <> "-" <> subcommand

enterEnv
    :: Text
    -> Params
    -> Maybe (FilePath, (Text, Env))
    -> Text
    -> HashMap Text Text
    -> IO ()
enterEnv stateEnvVar params possiblyEnv shellPid envVars = do
    let (file, hash, changes, applyEnv) = getChanges

    writeStateFile stateEnvVar params State
        { shellPid
        , config = File{file, hash} <$ possiblyEnv
        , changes
        }

    Lazy.Text.putStr applyEnv
    unless (Lazy.Text.null applyEnv) $ do
        printMsg params Info "Updating environment:"
        printMsg params Notice (Lazy.Text.toStrict applyEnv)
  where
    getChanges :: (Text, Text, [SetOrUnsetEnvVar], Lazy.Text)
    getChanges = maybe mempty getChanges' possiblyEnv

    getChanges'
        :: (FilePath, (Text, Env))
        -> (Text, Text, [SetOrUnsetEnvVar], Lazy.Text)
    getChanges' (file, (hash, e)) =
        let envDir = fromString (takeDirectory file)
            (ops, apply) = Bash.applyEnv envDir envVars e
        in  ( fromString file
            , hash
            , ops
            , apply
            )

writeStateFile :: Text -> Params -> State -> IO ()
writeStateFile stateEnvVar params state =
    withStateFile params $ \stateFile h -> do
        writeState h state

        -- Expose the new state file.
        Lazy.Text.putStr (Bash.setState stateEnvVar stateFile)

-- | State file is created with the following path:
--
-- > ${XDG_CACHE_HOME:-${HOME}/.cache}/${name}-${subcommand}/state${RANDOM}.dhall
--
-- Where @name@ and @subcommand@ are taken from 'Params', and @${RANDOM}@ is a
-- random string regenerated each time this function is called.
withStateFile :: Params -> (FilePath -> Handle -> IO a) -> IO a
withStateFile Params{name, subcommand} action = do
    cacheDir <- getXdgDirectory XdgCache (name <> "-" <> subcommand)

    -- There is no requirement for the cache directory to already exist.
    -- Function `getXdgDirectory` just constructs the path by following XDG
    -- Base Directory rules, nothing else.
    Turtle.mktree (Turtle.fromString cacheDir)

    openTempFile cacheDir "state.dhall" `bracket` (hClose . snd)
        $ uncurry action

-- | Print shell commands to standard output and inform user about those
-- changes by printing them to terminal as well.
leaveEnv
    :: Params
    -> State
    -> HashMap Text Text
    -> IO (HashMap Text Text)
leaveEnv params State{changes} e = do
    let restore = Lazy.Text.toStrict . genBash $ Bash.reverseOperations changes

    unless (null changes) $ do
        printMsg params Info "Restoring environment:"
        printMsg params Notice restore

    Env.apply changes e <$ Text.putStr restore

-- ----------------------------------------------------------------------------

data MessageType
    = Info
    | Notice
    | Warning
    | Error
  deriving stock (Eq, Show)

shouldPrintMessage :: Verbosity -> MessageType -> Bool
shouldPrintMessage verbosity = (verbosity >=) . \case
    Info -> Annoying
    Notice -> Verbose
    Warning -> Normal
    Error -> Normal

printMsg :: Params -> MessageType -> Text -> IO ()
printMsg params@Params{verbosity} messageType
  | shouldPrintMessage' = printMsg'
  | otherwise           = const (pure ())
  where
    printMsg' msg =
        withFile "/dev/tty" WriteMode $ \h -> do
            withColours h $ Text.hPutStr h (formatMsg msg)
            hPutStrLn h ""

    shouldPrintMessage' = shouldPrintMessage verbosity messageType

    formatMsg msg
      | messageType == Warning || messageType == Error
      = gshow messageType <> ": " <> msg

      | otherwise
      = msg

    withColours h m = do
        useColours' <- useColours params

        when useColours'
            . hPutStr h $ AnsiTerminal.setSGRCode (messageColour messageType)

        () <- m

        when useColours'
            . hPutStr h $ AnsiTerminal.setSGRCode [AnsiTerminal.Reset]

messageColour :: MessageType -> [AnsiTerminal.SGR]
messageColour = \case
    Info -> []
    Notice -> vividColor AnsiTerminal.White
    Warning -> vividColor AnsiTerminal.Yellow
    Error -> vividColor AnsiTerminal.Red
  where
    vividColor c =
        [AnsiTerminal.SetColor AnsiTerminal.Foreground AnsiTerminal.Vivid c]

useColours :: Params -> IO Bool
useColours Params{colour} =
    -- TODO: This is very naive. We need to make sure that the handle that we
    -- are writting into is attached to a terminal.
    useColoursWhen (terminalSupportsColours <$> setupTermFromEnv) colour

gshow :: (IsString s, Show a) => a -> s
gshow = fromString . show
