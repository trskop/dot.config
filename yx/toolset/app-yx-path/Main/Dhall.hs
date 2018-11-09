{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module:      Main.Dhall
-- Description: Generate Dhall configuration file with XDG Directories.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Generate Dhall configuration file with XDG Directories.
module Main.Dhall
    ( Options(..)
    , Output(..)
    , dhall
    )
  where

import Control.Exception (Exception, throwIO)
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Numeric.Natural (Natural)

import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Except (liftEither, runExcept, withExcept)
import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn, readFile)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import qualified Dhall
import qualified Dhall.Core as Dhall
    ( Expr
        ( Annot
        , App
        , Integer
        , Lam
        , Let
        , List
        , Natural
        , Record
        , RecordLit
        , Text
        )
    , Import
    , normalize
    )
import qualified Dhall.Import as Dhall (emptyStatus, loadWith)
import qualified Dhall.Map (fromList)
import qualified Dhall.Parser as Dhall (Src, exprFromText)
import qualified Dhall.TypeCheck as Dhall (TypeError, X, typeOf)
import System.FilePath (takeDirectory)
import System.Exit (die)

import Main.Paths (Paths)


data Output = Plain | DhallExpression | DhallType

data Options = Options
    { output :: Output
    }

dhall :: Options -> Paths -> Maybe FilePath -> Text -> IO ()
dhall Options{output} paths possiblyConfigFile expressionText = do
    config <- for possiblyConfigFile Text.readFile
    expression <- parseDhallExpression Nothing expressionText
    configExpression <- for config (parseDhallExpression possiblyConfigFile)
    case output of
        Plain ->
            printPlain paths expression configExpression

        DhallExpression ->
            printDhall paths expression configExpression fst

        DhallType ->
            printDhall paths expression configExpression snd

printDhall
    :: Paths
    -> Dhall.Expr Dhall.Src Dhall.X
    -> Maybe (Dhall.Expr Dhall.Src Dhall.X)
    ->  ( (Dhall.Expr Dhall.Src Dhall.X, Dhall.Expr Dhall.Src Dhall.X)
        -> Dhall.Expr Dhall.Src Dhall.X
        )
    -> IO ()
printDhall paths expression possiblyConfigExpression selector = do
    throwLeft mkExpressionAndTypeCheck' >>= putDoc . pretty . selector
    Text.putStrLn ""
  where
    mkExpressionAndTypeCheck' =
        mkExpressionAndTypeCheck Nothing expression configExpression paths

    configExpression = fromMaybe emptyRecord possiblyConfigExpression

printPlain
    :: Paths
    -> Dhall.Expr Dhall.Src Dhall.X
    -> Maybe (Dhall.Expr Dhall.Src Dhall.X)
    -> IO ()
printPlain paths expression possiblyConfigExpression =
    either dieWithNotPlainType id result
  where
    result :: Either [Dhall.TypeError Dhall.Src Dhall.X] (IO ())
    result = runExcept $ asum
        [ printText <$> mkExpression Dhall.Text
        , printNatural <$> mkExpression Dhall.Natural
        , printInteger <$> mkExpression Dhall.Integer
        , printTextList <$> mkExpression (listOf Dhall.Text)
        , printNaturalList <$> mkExpression (listOf Dhall.Natural)
        , printIntegerList <$> mkExpression (listOf Dhall.Integer)
        ]

    listOf = (Dhall.List `Dhall.App`)

    configExpression = fromMaybe emptyRecord possiblyConfigExpression

    mkExpression t = withExcept pure . liftEither
        $ fst <$> mkExpressionAndTypeCheck (Just t) expression configExpression paths

    dieWithNotPlainType _ = die
        "Error: Expected Text, Natural, Integer, or List of them as result."

    dieWithUnexpectedType t =
        die ("Error: Expected " <> t <> " as result.")

    Dhall.Type{extract = extractText} = Dhall.auto @Text
    Dhall.Type{extract = extractNatural} = Dhall.auto @Natural
    Dhall.Type{extract = extractInteger} = Dhall.auto @Integer
    Dhall.Type{extract = extractTextList} = Dhall.auto @[Text]
    Dhall.Type{extract = extractNaturalList} = Dhall.auto @[Natural]
    Dhall.Type{extract = extractIntegerList} = Dhall.auto @[Integer]

    printText, printNatural, printInteger
        :: Dhall.Expr Dhall.Src Dhall.X
        -> IO ()

    printText =
        maybe (dieWithUnexpectedType "Text") Text.putStrLn . extractText

    printNatural =
        maybe (dieWithUnexpectedType "Natural") (Text.putStrLn . showing)
        . extractNatural

    printInteger =
        maybe (dieWithUnexpectedType "Integer") (Text.putStrLn . showing)
        . extractInteger

    printTextList =
        maybe (dieWithUnexpectedType "List Text") (mapM_ Text.putStrLn)
        . extractTextList

    printNaturalList =
        maybe (dieWithUnexpectedType "List Natural")
            (mapM_ $ Text.putStrLn . showing)
        . extractNaturalList

    printIntegerList =
        maybe (dieWithUnexpectedType "List Integer")
            (mapM_ $ Text.putStrLn . showing)
        . extractIntegerList

    showing :: Show a => a -> Text
    showing = fromString . show

-- | Parse Dhall expression and resolve imports.
--
-- TODO:
--
-- * Support parsing a file
-- * Allow disabling imports
-- * Pass root directory for imports resolution.
parseDhallExpression :: Maybe FilePath -> Text -> IO (Dhall.Expr Dhall.Src Dhall.X)
parseDhallExpression possiblySourcePath =
    either throwIO resolveImports . Dhall.exprFromText sourcePath
  where
    sourcePath = fromMaybe "(command-line)" possiblySourcePath
    sourceDir = maybe "." takeDirectory possiblySourcePath

    resolveImports
        :: Dhall.Expr Dhall.Src Dhall.Import
        -> IO (Dhall.Expr Dhall.Src Dhall.X)
    resolveImports expr =
        evalStateT (Dhall.loadWith expr) (Dhall.emptyStatus sourceDir)

-- | Takes a Dhall @EXPRESSION@ and returns a following expression:
--
-- @
-- let
--     -- Type of 'Paths' data type so that it can be referred to in
--     -- EXPRESSION.
--     Paths = ...
--
-- in let
--     -- Type of 'Paths' data type so that it can be referred to in
--     -- EXPRESSION.
--     Config = ...
--
-- in
--     ( λ(data : {paths : Paths, config : Config}) → EXPRESSION : TYPE
--     ) data
-- @
mkExpressionAndTypeCheck
    :: Maybe (Dhall.Expr Dhall.Src Dhall.X)
    -- ^ Expected type of the result.
    -> Dhall.Expr Dhall.Src Dhall.X
    -- ^ Expression from command line.
    -> Dhall.Expr Dhall.Src Dhall.X
    -- ^ Parsed configuration file.
    -> Paths
    -> Either
        (Dhall.TypeError Dhall.Src Dhall.X)
        (Dhall.Expr Dhall.Src Dhall.X, Dhall.Expr Dhall.Src Dhall.X)
mkExpressionAndTypeCheck possiblyResultType bodyExpression configExpression paths = do
    expression <- mkExpression <$> Dhall.typeOf configExpression
    Dhall.typeOf expression <&> (Dhall.normalize expression, )
  where
    Dhall.InputType{..} = Dhall.inject @Paths

    bodyExpression' =
        maybe bodyExpression (Dhall.Annot bodyExpression) possiblyResultType

    mkExpression configExpressionType =
        Dhall.Let "Paths" Nothing declared
            ( Dhall.Let "Config" Nothing configExpressionType
                ( Dhall.Lam "data"
                    ( recordType
                        [ ("paths", declared)
                        , ("config", configExpressionType)
                        ]
                    )
                    bodyExpression'
                )
            )
        `Dhall.App` record
            [ ("paths", embed paths)
            , ("config", configExpression)
            ]

throwLeft :: Exception e => Either e r -> IO r
throwLeft = either throwIO pure

emptyRecord :: Dhall.Expr s a
emptyRecord = Dhall.RecordLit mempty

record :: [(Text, Dhall.Expr s a)] -> Dhall.Expr s a
record = Dhall.RecordLit . Dhall.Map.fromList

recordType :: [(Text, Dhall.Expr s a)] -> Dhall.Expr s a
recordType = Dhall.Record . Dhall.Map.fromList
