{-# LANGUAGE OverloadedLists #-}
-- |
-- Module:      Main.State
-- Description: Representation of environment state
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Representation of environment state, i.e. changes made to the environment so
-- that we can detect changes in configuration, and revert them if necessary.
module Main.State
    ( State(..)
    , File(..)
    , emptyState
    , readState
    , writeState
    )
  where

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Data.Functor ((<$))
import Data.Void (Void)
import GHC.Generics (Generic)
import System.IO (Handle)

import Control.Monad.State.Strict (evalStateT)
import Data.Either.Validation (validationToEither)
import Data.Text (Text)
import qualified Data.Text.IO as Text (readFile)
import qualified Dhall
    ( Decoder(expected, extract)
    , Encoder(embed)
    , FromDhall
    , ToDhall
    , auto
    , inject
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
        , Let
        , Var
        )
    , Import
    , normalize
    , throws
    )
import qualified Dhall.Import as Dhall (emptyStatus, loadWith)
import qualified Dhall.Parser as Dhall (Src, exprFromText)
import qualified Dhall.TypeCheck as Dhall (TypeError, typeOf)
import System.FilePath (takeDirectory)
import qualified Turtle

import qualified Main.Dhall as Dhall (hPut)
import Main.Env (SetOrUnsetEnvVar(..))


data File = File
    { file :: Text
    , hash :: Text
    -- ^ This is so that we can detect changes in the file.  If file is a Dhall
    -- config then we are using Dhall-specific content-aware hash algorithm.
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

data ActionState = ActionState
    { files :: [File]
    -- ^ Files to monitor for changes.  If one of them changed then we need to
    -- revert the changes to the environment, reexecute the command, and apply
    -- new set of changes.  Command should be executed in an environment with
    -- reverted changes.

--  , command :: ExecCommand
    -- ^ Command to execute.  We are expecting it to return a Dhall config
    -- describing environment changes.  If we fail to parse it we should print
    -- a sensible message to the user and a path to a file (in @\/tmp@?) where
    -- the output is stored.
    --
    -- Since environment changes are use shell-independent representatin we can
    -- use scripts that operate in any shell without the need of porting.

    , changes :: [SetOrUnsetEnvVar]
    -- ^ Changes introduced by an invoked action.
    }
  deriving stock (Eq, Generic)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

data State = State
    { shellPid :: Text
    , config :: Maybe File
    , changes :: [SetOrUnsetEnvVar]
--  , actions :: ActionState  -- TODO
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

emptyState :: State
emptyState = State
    { shellPid = ""
    -- This is OK.  Application will detect that empty string is not equal to
    -- the current shell PID, and it will create a new one with a valid
    -- 'shellPid' value.

    , config = Nothing
    , changes = []
    }

readState :: FilePath -> IO State
readState stateFile = do
    stateExists <- Turtle.testfile (Turtle.fromString stateFile)
    if stateExists
        then do
            stateContent <- Text.readFile stateFile
            expression <- parseDhallExpression stateFile stateContent
            throws (Dhall.extract Dhall.auto expression)
        else
            pure emptyState
  where
    throws = Dhall.throws . validationToEither

parseDhallExpression
    :: FilePath
    -> Text
    -> IO (Dhall.Expr Dhall.Src Void)
parseDhallExpression sourcePath = parseState >=> interpretState
  where
    parseState = either throwIO resolveImports . Dhall.exprFromText sourcePath

    interpretState =
        either throwIO pure . mkFullExpressionAndTypeCheck

    sourceDir = takeDirectory sourcePath

    resolveImports
        :: Dhall.Expr Dhall.Src Dhall.Import
        -> IO (Dhall.Expr Dhall.Src Void)
    resolveImports expr =
        evalStateT (Dhall.loadWith expr) (Dhall.emptyStatus sourceDir)

-- | Takes a Dhall @EXPRESSION@ and returns a following expression:
--
-- @
-- let State : Type = ...
-- let empty : State = ...
--
-- in  EXPRESSION : TYPE
-- @
mkFullExpressionAndTypeCheck
    :: Dhall.Expr Dhall.Src Void
    -- ^ Parsed configuration file.
    -> Either
        (Dhall.TypeError Dhall.Src Void)
        (Dhall.Expr Dhall.Src Void)
mkFullExpressionAndTypeCheck configExpression = do
    let expression = mkFullExpression configExpression
    Dhall.normalize expression <$ Dhall.typeOf expression
  where
    -- TODO: Switch to a normal import instead.  With custom context env
    -- configs are unable to import one another.
    mkFullExpression
        :: Dhall.Expr Dhall.Src Void
        -> Dhall.Expr Dhall.Src Void
    mkFullExpression expr =
        Dhall.Let
            Dhall.Binding
                { Dhall.variable = "State"
                , Dhall.annotation = Just (Nothing, Dhall.Const Dhall.Type)
                , Dhall.value = Dhall.expected (Dhall.auto @State)
                , Dhall.bindingSrc0 = Nothing
                , Dhall.bindingSrc1 = Nothing
                , Dhall.bindingSrc2 = Nothing
                }
            ( Dhall.Let
                Dhall.Binding
                    { Dhall.variable = "empty"
                    , Dhall.annotation = Just (Nothing, Dhall.Var "State")
                    , Dhall.value =
                        emptyStateExpr `Dhall.Annot` Dhall.Var "State"
                    , Dhall.bindingSrc0 = Nothing
                    , Dhall.bindingSrc1 = Nothing
                    , Dhall.bindingSrc2 = Nothing
                    }
                (expr `Dhall.Annot` Dhall.Var "State")
            )

    emptyStateExpr = Dhall.embed Dhall.inject emptyState

writeState :: Handle -> State -> IO ()
writeState = Dhall.hPut
