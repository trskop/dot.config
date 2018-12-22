-- |
-- Module:      Main.Bash
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main.Bash
    ( applyEnv
    , setState
    , unsetState

    --
    , operations
    , reverseOperations
    )
  where

import Data.Foldable (fold)
import Data.String (fromString)

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import GenBashrc.Bash (Bash, genBash)
import qualified GenBashrc.Bash as Bash

import Main.Config (Env(..))
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
        Bash.line $ fold
            [ "export ", name, "=", value
            , maybe previouslyNotDefined (previouslyDefined name) originalValue
            ]

    UnsetEnv{name, value} ->
        Bash.line $ "unset " <> name <> previouslyDefined name value

  where
    previouslyDefined name value =
        " # Previously defined as " <> name <> "=" <> value

    previouslyNotDefined =
        " # Previously wasn't defined."

setState :: Text -> FilePath -> Lazy.Text
setState name file =
    genBash $ Bash.export (Bash.var name) (Just . Bash.str $ fromString file)

unsetState :: Text -> Lazy.Text
unsetState name = genBash $ Bash.line ("unset " <> name)
