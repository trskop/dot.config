-- |
-- Module:      Main
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018-2019 Peter Trško
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
import Control.Monad (join, when, unless)
import Data.Foldable (asum, fold, for_)
import Data.Functor ((<&>))
import qualified Data.List as List (find)
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Traversable (for)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO
    ( Handle
    , hClose
    , hPutStr
    , hPutStrLn
    , openTempFile
    , stderr
    , stdout
    )

import CommandWrapper.Prelude
    ( HaveCompletionInfo(completionInfoMode)
    , Params(Params, colour, config, name, subcommand, verbosity)
    , completionInfoFlag
    , dieWith
    , printOptparseCompletionInfoExpression
    , shouldUseColours
    , subcommandParams
    )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import Data.Text (Text)
import qualified Data.Text as Text (unpack, unwords)
import qualified Data.Text.IO as Text (hPutStr, putStr)
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
import System.FilePath ((</>), takeDirectory)
import qualified Turtle

import Main.Action
    ( defaultAction
    , diffAction
    , dumpAction
    , initAction
    , scriptAction
    , setEnvConfigStatusAction

    -- Utilities
    -- TODO: Refactor so that they don't need to be imported.
    , findEnvFile
    , gshow
    , mkEnvFileName
    , mkPreferencesFilePath
    , withTerminal
    )
import Main.Config.App (Config(..), readConfig)
import Main.Config.Env (Env, readEnvConfig)
import Main.Config.Preferences (Preferences(Preferences))
import qualified Main.Config.Preferences as Preferences
    ( KnownFile(..)
    , Preferences(..)
    , Status(..)
    , read
    )
import qualified Main.Shell.Bash as Bash
    ( applyEnv
    , reverseOperations
    , setEnvDir
    , setState
    , unsetEnvDir
    , unsetState
    )
import Main.Env (SetOrUnsetEnvVar, reverseOperation, summary)
import qualified Main.Env as Env (apply)
import Main.State (File(..), State(..), emptyState, readState, writeState)


main :: IO ()
main = do
    params@Params{config = configFile} <- subcommandParams
    config <- readConfig (dieMissingConfig params) configFile
    mode <- parseOptions config
    env params mode

dieMissingConfig :: Params -> FilePath -> IO a
dieMissingConfig params fp = do
    printMsg params Error
        $ "Missing configuration file " <> gshow fp
    exitFailure

dieMissingRuntimeDir :: Params -> IO a
dieMissingRuntimeDir params = do
    printMsg params Error
        "Neither 'YX_ENV_STATE_DIR' nor 'XDG_RUNTIME_DIR' is set, please\
        \ set either of them to a sensible value."
    exitFailure

data Mode a
    -- TODO: Command to show preferences.
    = RunHook Text a
    | Script a
    | DryRun FilePath a
    | Init FilePath a
    | Cleanup a
    | SetEnvConfigStatus Preferences.Status FilePath a
    | Dump FilePath a
    | Diff FilePath a
    | Default a
    | CompletionInfo
    | Completion Word [String]
    | Help
  deriving stock (Show)

instance HaveCompletionInfo (Mode a) where
    completionInfoMode = const CompletionInfo

switchMode :: (forall b. b -> Mode b) -> Mode cfg -> Mode cfg
switchMode f = \case
    RunHook _ cfg -> f cfg
    Script cfg -> f cfg
    DryRun _ cfg -> f cfg
    Init _ cfg -> f cfg
    Cleanup cfg -> f cfg
    SetEnvConfigStatus _ _ cfg -> f cfg
    Dump _ cfg -> f cfg
    Diff _ cfg -> f cfg
    Default cfg -> f cfg
    CompletionInfo -> CompletionInfo
    Completion i ws -> Completion i ws
    Help -> Help

switchMode1 :: (forall b. a -> b -> Mode b) -> a -> Mode cfg -> Mode cfg
switchMode1 f a = switchMode (f a)

-- TODO: Switch to pure @optparse-applicative@ to gain full control over the
-- TUI.
parseOptions :: Config -> IO (Mode Config)
parseOptions config = Turtle.options "env" options <&> ($ Default config)
  where
    options =asum
        [ completionInfoFlag

        , runHookMode <$> Turtle.optText "run-hook" 'r'
            "Execute shell hook.  This is usually called when shell prompt is\
            \ redrawn.  See also '--script' option."

        , hookScriptMode <$> Turtle.switch "script" 's'
            "Script to be included in shell configuration, e.g. '.bashrc'."

        , initMode <$> Turtle.switch "init" 'i'
            "Create env config in the current directory."

        , cleanupMode <$> Turtle.switch "cleanup" 'c' "Perform cleanup"

        , dryRunMode <$> Turtle.optText "dry-run" 'u'
            "Print out what would happen if we cded into specified directory."

        -- TODO: Support '--pin' option that would allow only env file if its
        -- hash matches.
        , setStatusMode Preferences.Allowed <$> Turtle.optText "allow" 'a'
            "Allow specified env config to be used to modify environment."

        , setStatusMode Preferences.Ignored <$> Turtle.optText "ignore" 'g'
            "Ignore specified env config instead of using it to modify\
            \ environment."

        , dumpMode <$> Turtle.optText "dump" 'p'
            "Dump current environment into a specified file."

        , diffMode <$> Turtle.optText "diff" 'd'
            "Diff current environment against existing dump."

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

    dumpMode :: Text -> Mode Config -> Mode Config
    dumpMode = switchMode1 Dump . Text.unpack

    diffMode :: Text -> Mode Config -> Mode Config
    diffMode = switchMode1 Diff . Text.unpack

env :: Params -> Mode Config -> IO ()
env params@Params{name, subcommand} = \case
    CompletionInfo ->
        printOptparseCompletionInfoExpression stdout

    Completion _ _ ->
        notYetImplemented

    Help ->
        notYetImplemented

    Default config ->
        defaultAction params config

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

    Script config ->
        scriptAction params config

    RunHook shellPid config -> do
        let envFileName = mkEnvFileName params config
        printMsg params Info ("envFileName = " <> fromString (show envFileName))
        envFile' <- Turtle.pwd >>= findEnvFile envFileName
        printMsg params Info ("envFile' = " <> fromString (show envFile'))

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
        printMsg params Info ("stateFile = " <> fromString (show stateFile))
        state <- maybe (pure emptyState) (readState . Text.unpack) stateFile
        printMsg params Info ("state = " <> fromString (show state))

        possiblyParsedConfig <-
            maybe (pure Nothing) (\fp -> fmap (fp, ) <$> readEnvConfig fp)
                envFile

        let possiblyEnvFile =
                possiblyParsedConfig <&> \(file, (hash, _)) -> File
                    { file = fromString file
                    , hash
                    }

            shellChanged = hasShellChanged state shellPid
            stateStale = isStateStale state possiblyEnvFile

        -- We need to detect 'shellChanged' before we try to leave and enter a
        -- state if the current one is stale.  This is due to the fact that
        -- both of these (entered a subshell and chaned directory that got us
        -- in/out of a scope of an env file) may have happened at the same
        -- time.
        --
        -- Reason for not duplicating state when the state is stale is that it
        -- will be done for us by the 'enterEnv' function.  If we duplicated it
        -- and then entered a new state, then we would just end up creating a
        -- file that was completely unused, and we would have to provide quite
        -- complicated logic to make sure that it gets deleted at some point.
        when (shellChanged && not stateStale)
            -- Clone state file so that the state of the subshell can evolve
            -- independently without affecting its parent state.
            $ writeStateFile stateEnvVar params state{shellPid}

        -- Stale state is when we got in or out of scope of an env config.
        when stateStale $ do
            leaveEnv params state possiblyEnvFile envVars
                >>= enterEnv stateEnvVar params possiblyParsedConfig shellPid

            -- Function `enterEnv` created a new state file.  It is safe to
            -- delete the old one, unless we have entered a subshell.  In such
            -- case the old state is owned by the parent shell.
            unless shellChanged
                $ for_ stateFile
                    $ Turtle.rm . Turtle.fromText

    Init dir config ->
        initAction params config dir

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

    SetEnvConfigStatus status dir config ->
        setEnvConfigStatusAction params config status dir

    Dump file config ->
        dumpAction params config file

    Diff file config ->
        diffAction params config file
  where
    stateEnvVar = "YX_ENV_STATE" :: Text

    notYetImplemented =
        dieWith params stderr 125 "Bug: This should not happen at the moment."

isStateStale :: State -> Maybe File -> Bool
isStateStale State{config} currentConfig = config /= currentConfig

hasShellChanged :: State -> Text -> Bool
hasShellChanged State{shellPid} currentShellPid = shellPid /= currentShellPid

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
    for_ (fst <$> possiblyEnv)
        $ Lazy.Text.putStr . Bash.setEnvDir "YX_ENV_DIR" . takeDirectory

    unless (Lazy.Text.null applyEnv) $ do
        printMsg params Info "Updating environment:"
        printMsg params Notice $ Text.unwords (summary changes envVars)
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
-- > ${YX_ENV_STATE_DIR:-$XDG_RUNTIME_DIR}/${name}-${subcommand}/state${RANDOM}.dhall
--
-- Where @name@ and @subcommand@ are taken from 'Params', and @${RANDOM}@ is a
-- random string regenerated each time this function is called.
withStateFile :: Params -> (FilePath -> Handle -> IO a) -> IO a
withStateFile params@Params{name, subcommand} action = do

    -- TODO: Refactor this to avoid hardcoded values.
    stateDir <- lookupEnv "YX_ENV_STATE_DIR" >>= \case
        Nothing ->
            lookupEnv "XDG_RUNTIME_DIR" >>= \case
                Nothing ->
                    dieMissingRuntimeDir params

                Just xdgRuntimeDir -> do
                    let dir = xdgRuntimeDir </> (name <> "-" <> subcommand)
                    dir <$ Lazy.Text.putStr (Bash.setEnvDir "YX_ENV_STATE_DIR" dir)

        Just dir ->
            pure dir

    -- There is no requirement for the state directory to already exist.
    -- TODO: IMPORTANT: Set permissions to the directory to avoid unauthorised
    -- access.
    Turtle.mktree (Turtle.fromString stateDir)

    openTempFile stateDir "state.dhall" `bracket` (hClose . snd)
        $ uncurry action

-- | Print shell commands to standard output and inform user about those
-- changes by printing them to terminal as well.
leaveEnv
    :: Params
    -> State
    -> Maybe File
    -> HashMap Text Text
    -> IO (HashMap Text Text)
leaveEnv params State{changes} possiblyEnvFile envVars = do
    let restore = Lazy.Text.toStrict . genBash $ Bash.reverseOperations changes

    unless (null changes) $ do
        printMsg params Info "Restoring environment:"
        let report = summary (reverseOperation <$> changes) envVars
        printMsg params Notice (Text.unwords report)

    when (isNothing possiblyEnvFile)
        $ Lazy.Text.putStr (Bash.unsetEnvDir "YX_ENV_DIR")

    Env.apply changes envVars <$ Text.putStr restore

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
printMsg Params{colour, verbosity} messageType
  | shouldPrintMessage' = printMsg'
  | otherwise           = const (pure ())
  where
    printMsg' msg =
        withTerminal $ \h -> do
            withColours h $ Text.hPutStr h (formatMsg msg)
            hPutStrLn h ""

    shouldPrintMessage' = shouldPrintMessage verbosity messageType

    formatMsg msg
      | messageType == Warning || messageType == Error
      = gshow messageType <> ": " <> msg

      | otherwise
      = msg

    withColours h m = do
        useColours <- shouldUseColours h colour

        when useColours
            . hPutStr h $ AnsiTerminal.setSGRCode (messageColour messageType)

        () <- m

        when useColours
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
