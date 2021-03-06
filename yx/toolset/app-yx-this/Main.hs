-- |
-- Module:      Main
-- Description: TODO: Description
-- Copyright:   (c) 2018-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Description
module Main (main)
  where

import Control.Applicative ((<|>), many)
import Data.Foldable (asum)
import Data.Monoid (Endo(..), mempty)

import CommandWrapper.Subcommand.Prelude
    ( HaveCompletionInfo(completionInfoMode)
    , Params
    , completionInfoFlag
    , dieWith
    , printOptparseCompletionInfoExpression
    , stderr
    , stdout
    , subcommandParams
    )
import Data.Monoid.Endo.Fold (dualFoldEndo)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert)
import Mainplate (applySimpleDefaults, runAppWith)
import qualified Options.Applicative as Options
import qualified Turtle

import Main.Action (EditWhat(..), updateAction, editAction, defaultAction)
import Main.Config.App (Config, UpdateWhat(..))
import qualified Main.Config.App as Config (parse, printDef)


-- TODO:
--
-- Features provided by the original `this` script that would be nice to
-- preserve:
--
-- - `--edit` [--fzf] [CONFIG_SELECTOR] -- Fast way to open a configuration
--   file in editor.  For some of them it understood that there is a need to
--   run an action.  Would it make sense to make this into a separate
--   subcommand?  Maybe merge with with `yx jmp`?  However, if those action
--   hooks should be working then it will need to call this one as well.  BTW,
--   CTRL+e is not binded in current Bash setup.
--
-- - Management of `apt/sources.list` and extra package repositories.
--
-- - Generate SSH key, store it in a password DB allong with randomly generated
--   password that was used to encrypt it.  Would it make sense to make this a
--   separate subcommand?
--
-- - Set alternatives (`update-alternatives`) on Debian/Ubuntu.
--
-- Features that weren't finished but would be nice to have them:
--
-- - Integration with etckeeper.  If there were changes made to package
--   configuration it made sense to create a commit message for `/etc` changes.
--   Similar thing for when pinning packages to a specific version due to
--   outstanding issues.
--
-- - `--bootstrap-script` option to generate a bootstraping script that
--   prepares newly installed system into a state where:
--
--     * Necessary packages are installed.
--     * `~/.config` is initialised.
--     * `~/.bashrc` modified appropriately.
--     * Command `yx this` is able to run.
--
--     This was partially implemented during first attempt at a rewrite into
--     Haskell.
--
-- - Hint system.  If there is a file/directory/command missing that it is not
--   able to create/install then it will produce a "hint message" where it
--   describes how to create/install it.  E.g. missing identity file for a
--   specific purpose.
--
-- - Allow Debian and Ubuntu coexist in top-level configuration file.  Instead
--   of package list we could use a function that produces it based on OS info.
--
-- Things that would be interesting to consider:
--
-- - Allowing user to select actions from `--update --user` subset by not only
--   listing executed scripts but also querying Shake scripts for their
--   targets.
--
-- - Integrate `apt search`, `apt show`, `dpkg-query -[LS]`, etc.  This would
--   allow something like:
--
--     ```
--     function _ditch_apt() {
--         echo "Use 'this' or 'yx this' instead of calling 'apt' directly!" 1>&2
--     }
--     alias apt='_ditch_apt'
--     ```
--
-- - Integrate with Nix.
--
-- - Change interface to something like:
--
--     ```
--     yx this u[pdate] [WHAT_TO_UPDATE]
--     yx this s[earch] [PACKAGE_PATTERN]
--     yx this [sho]w {[--list-files] PACKAGE_NAME|--file=FILE}
--     yx this e[dit] [WHAT_TO_EDIT]
--     yx this b[ackup] [WHAT_TO_EDIT]
--     ```
--
-- - Backup functionality.  Just a simple wrapper for calling backup command
--   with correct arguments.  List of files/directories for backup will be in
--   `~/.config/yx/yx-this.dhall`.

main :: IO ()
main = do
    params <- subcommandParams
    runAppWith parseOptions (parseConfig params) applyDefaults $ \case
        Update what (Just config) ->
            updateAction params config what

        Edit what (Just config) ->
            editAction params config what

        PrintDefaultConfig _ ->
            Config.printDef

        Default (Just config) ->
            defaultAction params config

        CompletionInfo ->
            printOptparseCompletionInfoExpression stdout

        Completion _ _ ->
            notYetImplemented params

        Help ->
            notYetImplemented params

        Update _ Nothing ->
            missingConfig params

        Edit _ Nothing ->
            missingConfig params

        Default Nothing ->
            missingConfig params
  where
    missingConfig params =
        dieWith params stderr 1
            "Configuration file is missing, you can use '--default-config' to\
            \ generate initial value."

    notYetImplemented params =
        dieWith params stderr 125 "Bug: This should not happen at the moment."

parseConfig :: Params -> mode a -> IO (Either String (Endo (Maybe Config)))
parseConfig params _ = Right . Endo . const <$> Config.parse params

data Mode config
    = Update (Set UpdateWhat) config
    | Edit EditWhat config
    | PrintDefaultConfig config
    | Default config
    | CompletionInfo
    | Completion Word [String]
    | Help
  deriving stock (Functor, Show)

instance HaveCompletionInfo (Mode config) where
    completionInfoMode = const CompletionInfo

switchMode :: (forall x. x -> Mode x) -> Endo (Mode config)
switchMode f = Endo $ \case
    Update _ config -> f config
    Edit _ config -> f config
    PrintDefaultConfig config -> f config
    Default config -> f config
    CompletionInfo -> CompletionInfo
    Completion i ws -> Completion i ws
    Help -> Help

switchMode1 :: (forall x. a -> x -> Mode x) -> a -> Endo (Mode config)
switchMode1 f a = switchMode (f a)

applyDefaults :: Endo (Mode (Maybe Config)) -> IO (Mode (Maybe Config))
applyDefaults = applySimpleDefaults (Default Nothing)

parseOptions :: IO (Endo (Mode config))
parseOptions = Turtle.options "TODO: Describe me!" options

options :: Options.Parser (Endo (Mode config))
options = asum
    [ Endo <$> completionInfoFlag
    , dualFoldEndo
        <$> updateFlag
        <*> many
            ( systemFlag
            <|> installFlag
            <|> userFlag
            <|> nixFlag
            )

    , editFlag
    , defaultConfigFlag

    , pure mempty
    ]

updateFlag :: Options.Parser (Endo (Mode config))
updateFlag = Options.flag' (switchMode1 Update Set.empty) $ mconcat
    [ Options.short 'U'
    , Options.long "update"
    , Options.help "Update"
    ]

systemFlag :: Options.Parser (Endo (Mode config))
systemFlag = Options.flag' (addUpdateWhat UpdateSystem) $ mconcat
    [ Options.short 's'
    , Options.long "system"
    , Options.help "Update system"
    ]

installFlag :: Options.Parser (Endo (Mode config))
installFlag = Options.flag' (addUpdateWhat InstallPackages) $ mconcat
    [ Options.short 'i'
    , Options.long "install"
    , Options.help "Install packages"
    ]

userFlag :: Options.Parser (Endo (Mode config))
userFlag = Options.flag' (addUpdateWhat UpdateUserEnvironment) $ mconcat
    [ Options.short 'u'
    , Options.long "user"
    , Options.help "Update user environment"
    ]

nixFlag :: Options.Parser (Endo (Mode config))
nixFlag = Options.flag' (addUpdateWhat UpdateNixEnvironment) $ mconcat
    [ Options.short 'n'
    , Options.long "nix"
    , Options.help "Update nix environment"
    ]

addUpdateWhat :: UpdateWhat -> Endo (Mode config)
addUpdateWhat w = Endo $ \case
    Update what config -> Update (Set.insert w what) config
    mode -> mode

editFlag :: Options.Parser (Endo (Mode config))
editFlag = Options.flag' (switchMode1 Edit ThisConfigFile) $ mconcat
    [ Options.short 'e'
    , Options.long "edit"
    , Options.help "Edit"
    ]

defaultConfigFlag :: Options.Parser (Endo (Mode config))
defaultConfigFlag = Options.flag' (switchMode PrintDefaultConfig) $ mconcat
    [ Options.long "init-config"
    , Options.help "Print initial configuration file if it doesn't exist"
    ]
