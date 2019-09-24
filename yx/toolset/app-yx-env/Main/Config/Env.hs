{-# LANGUAGE OverloadedLists #-}
-- |
-- Module:      Main.Config.Env
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main.Config.Env
    (
    -- * Env Config
      EnvironmentVariable(..) -- Re-exported
    , EnvironmentVariableOperation(..)
    , Env(..)
    , readEnvConfig
    )
  where

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Data.Monoid (mempty)
import Data.Functor ((<$))
import Data.String (fromString)
import GHC.Generics (Generic)

import Control.Monad.State.Strict (evalStateT)
import Data.Either.Validation (validationToEither)
import Data.Text (Text)
import qualified Data.Text.IO as Text (readFile)
import qualified Dhall
    ( Inject
    , Interpret
    , Type(expected, extract)
    , auto
    )
import qualified Dhall.Core as Dhall
    ( Binding
        ( Binding
        , annotation
        , bindingSrc0
        , bindingSrc1
        , bindingSrc2
        , value
        , variable
        )
    , Const(Type)
    , Expr
        ( Annot
        , Const
        , Lam
        , Let
        , ListLit
        , RecordLit
        , Text
        , Var
        )
    , Import(..)
    , ImportHashed(..)
    , ImportMode(Code)
    , ImportType(Missing)
    , normalize
    , throws
    )
import qualified Dhall.Import as Dhall (emptyStatus, hashExpression, loadWith)
import qualified Dhall.Map (fromList)
import qualified Dhall.Parser as Dhall (Src, exprFromText)
import qualified Dhall.TypeCheck as Dhall (X, typeOf)
import System.Environment.Variable
    ( EnvironmentVariable(..)
    , EnvVarName
    , EnvVarValue
    )
import System.FilePath (takeDirectory)
import qualified Turtle


data EnvironmentVariableOperation
    = Set
        { name :: EnvVarName
        , value :: EnvVarValue
        }
    | Unset
        { name :: EnvVarName
        }
    | Modify
        { name :: EnvVarName
        , modify :: Maybe EnvVarValue -> Maybe EnvVarValue
        }
  deriving stock (Generic)
  deriving anyclass (Dhall.Interpret)

data ReversibleAction = ReversibleAction
    { name :: Text
    , action :: Text -- TODO: Command
--  , operations :: [SetOrUnsetEnvVar]
--  , revereOperations :: [SetOrUnsetEnvVar]
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

data Env = Env
    { variables :: Text -> [EnvironmentVariableOperation]
    , actions :: [ReversibleAction]
    }
  deriving stock (Generic)
  deriving anyclass (Dhall.Interpret)

readEnvConfig :: FilePath -> IO (Maybe (Text, Env))
readEnvConfig configFile = do
    configExists <- Turtle.testfile (Turtle.fromString configFile)
    if configExists
        then do
            configContent <- Text.readFile configFile
            expression <- parseDhallExpression configFile configContent
            v <- throws (Dhall.extract Dhall.auto expression)
            pure $ Just (mkHash expression, v)

        else
            pure Nothing
  where
    mkHash = fromString . show . Dhall.hashExpression

    throws = Dhall.throws . validationToEither

parseDhallExpression :: FilePath -> Text -> IO (Dhall.Expr Dhall.Src Dhall.X)
parseDhallExpression sourcePath = parseConfig >=> typeCheckAndNormalize
  where
    parseConfig =
        -- We need to resolve imports after 'mkFullExpression' so that
        -- 'mkFullExpression' can introduce imports if necessary.
        either throwIO (resolveImports . mkFullExpression)
        . Dhall.exprFromText sourcePath

    resolveImports
        :: Dhall.Expr Dhall.Src Dhall.Import
        -> IO (Dhall.Expr Dhall.Src Dhall.X)
    resolveImports expr =
        evalStateT (Dhall.loadWith expr) (Dhall.emptyStatus sourceDir)
      where
        sourceDir = takeDirectory sourcePath

    typeCheckAndNormalize expr =
        Dhall.normalize expr <$ either throwIO pure (Dhall.typeOf expr)

-- | Takes a Dhall @EXPRESSION@ and returns a following expression:
--
-- @
-- let Variable : Type = ...
-- let Action : Type = ...
-- ...
-- let Env : Type = ...
-- let empty : Env = ...
--
-- in  EXPRESSION : TYPE
-- @
mkFullExpression
    :: Dhall.Expr Dhall.Src Dhall.Import
    -- ^ Parsed configuration file.
    -> Dhall.Expr Dhall.Src Dhall.Import
mkFullExpression expr =
    -- TODO: Move these definitions into a library that can be imported.
    -- Locally bound values prohibit env configs to import one another.
    binding "Variable" (Dhall.Const Dhall.Type)
        ( allowImports $ Dhall.expected (Dhall.auto @EnvironmentVariable)
        )
    `let_` binding "Action" (Dhall.Const Dhall.Type)
        ( allowImports $ Dhall.expected (Dhall.auto @ReversibleAction)
        )
    `let_` binding "Actions" (Dhall.Const Dhall.Type)
        ( allowImports $ Dhall.expected (Dhall.auto @[ReversibleAction])
        )
    `let_` binding "VariableOperation" (Dhall.Const Dhall.Type)
        ( allowImports
            $ Dhall.expected (Dhall.auto @EnvironmentVariableOperation)
        )
    `let_` binding "VariableOperations" (Dhall.Const Dhall.Type)
        ( allowImports
            $ Dhall.expected (Dhall.auto @[EnvironmentVariableOperation])
        )
    `let_` binding "Env" (Dhall.Const Dhall.Type)
        ( allowImports $ Dhall.expected (Dhall.auto @Env)
        )
    `let_` binding "empty" (Dhall.Var "Env")
        ( record
            [ ( "variables"
              , Dhall.Lam "dir" Dhall.Text
                    (emptyListOf "VariableOperation")
              )

            , ("actions", emptyListOf "Action")
            ]
        )
    `let_`
        (expr `Dhall.Annot` Dhall.Var "Env")
  where
    binding variable ann value = Dhall.Binding
        { Dhall.variable
        , Dhall.annotation = Just (Nothing, ann)
        , Dhall.value
        , Dhall.bindingSrc0 = Nothing
        , Dhall.bindingSrc1 = Nothing
        , Dhall.bindingSrc2 = Nothing
        }

    emptyListOf :: String -> Dhall.Expr s a
    emptyListOf v = Dhall.ListLit (Just . Dhall.Var $ fromString v) mempty

    record :: [(Text, Dhall.Expr s a)] -> Dhall.Expr s a
    record = Dhall.RecordLit . Dhall.Map.fromList

    -- This should be safe, even unsafeCorece should be safe in this case since
    -- 'X' is 'Void', therefore, it is imposible to have 'Dhall.Embed' case.
    allowImports :: Dhall.Expr s Dhall.X -> Dhall.Expr s Dhall.Import
    allowImports = fmap
        $ const Dhall.Import
            { importHashed = Dhall.ImportHashed
                { hash = Nothing
                , importType = Dhall.Missing
                }
            , importMode = Dhall.Code
            }

    let_ = Dhall.Let
    infixr 9 `let_`
