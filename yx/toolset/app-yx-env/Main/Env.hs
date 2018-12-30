-- |
-- Module:      Main.Env
-- Description: Algebra of environment modifications.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Algebra of environment modifications.
module Main.Env
    ( SetOrUnsetEnvVar(..)
    , simplifyOperation
    , simplifyOperations
    , reverseOperation
    , apply
    , diff
    )
  where

import Data.Function (on)
import Data.Functor ((<&>))
import qualified Data.List as List (sort)
import Data.Maybe (mapMaybe)
import Data.Monoid (Endo(..))
import GHC.Generics (Generic)

import Data.Algorithm.Diff (Diff(..), getDiffBy)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (delete, insert, lookup)
import qualified Dhall (Inject, Interpret)
import System.Environment.Variable
    ( EnvironmentVariable(..)
    , EnvVarName
    , EnvVarValue
    )

import Main.Config.Env (EnvironmentVariableOperation(..))


-- | Invertible operations over environment variables.
data SetOrUnsetEnvVar
    = SetEnv
        { name :: EnvVarName
        , value :: EnvVarValue
        , originalValue :: Maybe EnvVarValue
        }
    | UnsetEnv
        { name :: EnvVarName
        , value :: EnvVarValue
        -- ^ Value that is being unset, similar purpose as 'originalValue' in
        -- case of 'SetEnv'.
        }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

-- | Take user-specified fancy operation and reduce it to a simple
-- 'SetEnv'\/'UnsetEnv'.
simplifyOperation
    :: HashMap EnvVarName EnvVarValue
    -> EnvironmentVariableOperation
    -> Maybe SetOrUnsetEnvVar
    -- ^ 'Nothing' means no changes to be performed.
simplifyOperation env = \case
    Set EnvironmentVariable{name, value} ->
        let originalValue = HashMap.lookup name env
        in if originalValue == Just value
            then Nothing
            else Just SetEnv{name, value, originalValue}

    Unset name ->
        HashMap.lookup name env <&> \value ->
            UnsetEnv{name, value}

    Modify name f ->
        modify name (HashMap.lookup name env) f
  where
    modify name originalValue f =
        case (originalValue, f originalValue) of
            (Nothing, Nothing) ->
                Nothing

            (Nothing, Just value) ->
                Just SetEnv{name, value, originalValue}

            (Just value, Nothing) ->
                Just UnsetEnv{name, value}

            (Just originalValue', Just value)
              | originalValue' == value ->
                    Nothing

              | otherwise ->
                    Just SetEnv{name, value, originalValue}

simplifyOperations
    :: HashMap EnvVarName EnvVarValue
    -> [EnvironmentVariableOperation]
    -> [SetOrUnsetEnvVar]
simplifyOperations env = mapMaybe (simplifyOperation env)

reverseOperation :: SetOrUnsetEnvVar -> SetOrUnsetEnvVar
reverseOperation = \case
    SetEnv{name, value, originalValue} ->
        case originalValue of
            Nothing ->
                UnsetEnv{name, value}

            Just originalValue' ->
                SetEnv{name, value = originalValue', originalValue = Just value}

    UnsetEnv{name, value} ->
        SetEnv{name, value, originalValue = Nothing}

apply
    :: [SetOrUnsetEnvVar]
    -> HashMap EnvVarName EnvVarValue
    -> HashMap EnvVarName EnvVarValue
apply = appEndo . foldMap (Endo . applyOperation)

applyOperation
    :: SetOrUnsetEnvVar
    -> HashMap EnvVarName EnvVarValue
    -> HashMap EnvVarName EnvVarValue
applyOperation = \case
    SetEnv{name, value} ->
        HashMap.insert name value

    UnsetEnv{name} ->
        HashMap.delete name

diff
    :: [(EnvVarName, EnvVarValue)]
    -- ^ Old environment.
    -> [(EnvVarName, EnvVarValue)]
    -- ^ New environment.
    -> [SetOrUnsetEnvVar]
    -- ^ Operations that will transform old environment into the new one.
diff old new =
    mapMaybe toOp $ getDiffBy ((==) `on` fst) (List.sort old) (List.sort new)
  where
    toOp = \case
        First (name, value) ->
            Just UnsetEnv{name, value}

        Second (name, value) ->
            Just SetEnv{name, value, originalValue = Nothing}

        Both (name, originalValue) (_, value)
          | originalValue == value ->
                Nothing

          | otherwise ->
                Just SetEnv{name, value, originalValue = Just originalValue}
