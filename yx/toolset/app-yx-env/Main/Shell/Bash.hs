-- |
-- Module:      Main.Shell.Bash
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main.Shell.Bash
    ( applyEnv
    , setState
    , unsetState
    , setEnvDir
    , unsetEnvDir

    --
    , operations
    , reverseOperations
    )
  where

import Data.Maybe (fromMaybe)
import Data.String (fromString)

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import GenBashrc.Bash (Bash, genBash)
import qualified GenBashrc.Bash as Bash

import Main.Config.Env (Env(..))
import Main.Env
    ( SetOrUnsetEnvVar(..)
    , reverseOperation
    , simplifyOperations
    )


-- TODO: Handle 'actions' field.
applyEnv
    :: Text
    -- ^ Directory where env config was found.
    -> HashMap Text Text
    -> Env
    -> ([SetOrUnsetEnvVar], Lazy.Text)
applyEnv dir env Env{..} =
    let ops = simplifyOperations env (variables dir)
    in  ( ops
        , genBash (operations ops)
        )

reverseOperations :: [SetOrUnsetEnvVar] -> Bash ()
reverseOperations = mapM_ (operation . reverseOperation)

operations :: [SetOrUnsetEnvVar] -> Bash ()
operations = mapM_ operation

operation :: SetOrUnsetEnvVar -> Bash ()
operation = \case
    SetEnv{name, value, originalValue} ->
        mapM_ Bash.line
            [ previouslyDefined name originalValue
            , "export " <> name <> "=" <> value
            ]

    UnsetEnv{name, value} ->
        mapM_ Bash.line
            [ previouslyDefined name (Just value)
            , "unset " <> name
            ]
  where
    previouslyDefined name value =
        "# " <> name <> "=" <> fromMaybe "" value

setState :: Text -> FilePath -> Lazy.Text
setState name file =
    genBash $ Bash.export (Bash.var name) (Just . Bash.str $ fromString file)

unsetState :: Text -> Lazy.Text
unsetState name = genBash $ Bash.line ("unset " <> name)

setEnvDir :: Text -> FilePath -> Lazy.Text
setEnvDir name dir =
    genBash $ Bash.export (Bash.var name) (Just . Bash.str $ fromString dir)

unsetEnvDir :: Text -> Lazy.Text
unsetEnvDir name = genBash $ Bash.line ("unset " <> name)
