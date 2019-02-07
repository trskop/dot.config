-- |
-- Module:      Main
-- Description: TODO: Description
-- Copyright:   (c) 2018-2019 Peter Trško
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
import Data.Foldable (asum, for_)
import Data.Monoid (Endo(..), mempty)
import Data.String (fromString)

import CommandWrapper.Prelude
    ( Params(Params, config)
    , dieWith
    , stderr
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
import qualified Main.Config.App as Config (read, writeDef)


-- TODO:
--
-- Features provided by the original `this` script that would be nice to
-- preserve:
--
-- - `--edit` [--fzf] [CONFIG_SELECTOR] -- Fast way to open a configuration
--   file in editor.  For some of them it understood that there is a need to
--   run an action.  Woid it make sense to make this into a separate
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

main :: IO ()
main = do
    params <- subcommandParams
    runAppWith parseOptions (readConfig params) applyDefaults $ \case
        Update what (Just config) ->
            updateAction params config what

        Edit what (Just config) ->
            editAction params config what

        CreateDefaultConfig possiblyConfig -> do
            for_ possiblyConfig $ \_ ->
                let Params{config} = params
                in dieWith params stderr 1
                    ( fromString (show config) <> ": Configuration file\
                    \ already exists, refusing to overwrite it."
                    )

            Config.writeDef params

        Default (Just config) ->
            defaultAction params config

        _ ->
            let Params{config} = params
            in dieWith params stderr 1
                ( fromString (show config) <> ": Configuration file is missing\
                \, you can use '--default-config' to generate initial value."
                )

readConfig :: Params -> mode a -> IO (Either String (Endo (Maybe Config)))
readConfig params _ = Right . Endo . const <$> Config.read params

data Mode config
    = Update (Set UpdateWhat) config
    | Edit EditWhat config
    | CreateDefaultConfig config
    | Default config
  deriving stock (Functor, Show)

switchMode :: (forall x. x -> Mode x) -> Endo (Mode config)
switchMode f = Endo $ \case
    Update _ config -> f config
    Edit _ config -> f config
    CreateDefaultConfig config -> f config
    Default config -> f config

switchMode1 :: (forall x. a -> x -> Mode x) -> a -> Endo (Mode config)
switchMode1 f a = switchMode (f a)

applyDefaults :: Endo (Mode (Maybe Config)) -> IO (Mode (Maybe Config))
applyDefaults = applySimpleDefaults (Default Nothing)

parseOptions :: IO (Endo (Mode config))
parseOptions = Turtle.options "TODO: Describe me!" options

options :: Options.Parser (Endo (Mode config))
options = asum
    [ dualFoldEndo
        <$> updateFlag
        <*> many
            ( systemFlag
            <|> installFlag
            <|> userFlag
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
defaultConfigFlag = Options.flag' (switchMode CreateDefaultConfig) $ mconcat
    [ Options.long "default-config"
    , Options.help "Create initial configuration file if it doesn't exist"
    ]
