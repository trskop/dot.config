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
import Numeric.Natural (Natural)

import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Except (liftEither, runExcept, withExcept)
import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import qualified Dhall
import qualified Dhall.Core as Dhall
    ( Expr(Annot, App, Integer, Lam, Let, Natural, Text)
    , Import
    , normalize
    )
import qualified Dhall.Import as Dhall (emptyStatus, loadWith)
import qualified Dhall.Parser as Dhall (Src, exprFromText)
import qualified Dhall.TypeCheck as Dhall (TypeError, X, typeOf)
import System.Exit (die)

import Main.Paths (Paths)


data Output = Plain | DhallExpression | DhallType

data Options = Options
    { output :: Output
    }

dhall :: Options -> Paths -> Text -> IO ()
dhall Options{output} paths expressionText = do
    expression <- parseDhallExpression expressionText
    case output of
        Plain ->
            printPlain paths expression

        DhallExpression ->
            printDhall paths expression fst

        DhallType ->
            printDhall paths expression snd

printDhall
    :: Paths
    -> Dhall.Expr Dhall.Src Dhall.X
    ->  ( (Dhall.Expr Dhall.Src Dhall.X, Dhall.Expr Dhall.Src Dhall.X)
        -> Dhall.Expr Dhall.Src Dhall.X
        )
    -> IO ()
printDhall paths expression selector = do
    throwLeft (mkExpressionAndTypeCheck Nothing expression paths)
        >>= putDoc . pretty . selector
    Text.putStrLn ""

printPlain :: Paths -> Dhall.Expr Dhall.Src Dhall.X -> IO ()
printPlain paths expression = either dieWithNotPlainType id result
  where
    result :: Either [Dhall.TypeError Dhall.Src Dhall.X] (IO ())
    result = runExcept $ asum
        [ printText <$> mkExpressionAndTypeCheck' Dhall.Text
        , printNatural <$> mkExpressionAndTypeCheck' Dhall.Natural
        , printInteger <$> mkExpressionAndTypeCheck' Dhall.Integer
        ]

    mkExpressionAndTypeCheck' t = withExcept pure . liftEither
        $ fst <$> mkExpressionAndTypeCheck (Just t) expression paths

    dieWithNotPlainType _ =
        die "Error: Expected Text, Natural or Integer as result."

    dieWithUnexpectedType t =
        die ("Error: Expected " <> t <> " as result.")

    Dhall.Type{extract = extractText} = Dhall.auto @Text
    Dhall.Type{extract = extractNatural} = Dhall.auto @Natural
    Dhall.Type{extract = extractInteger} = Dhall.auto @Integer

    printText, printNatural, printInteger
        :: Dhall.Expr Dhall.Src Dhall.X
        -> IO ()

    printText =
        maybe (dieWithUnexpectedType "Text") Text.putStrLn . extractText

    printNatural =
        maybe (dieWithUnexpectedType "Natural") (Text.putStrLn . showing)
        . extractNatural

    printInteger =
        maybe (dieWithUnexpectedType "Natural") (Text.putStrLn . showing)
        . extractInteger

    showing :: Show a => a -> Text
    showing = fromString . show

-- | Parse Dhall expression and resolve imports.
--
-- TODO:
--
-- * Support parsing a file
-- * Allow disabling imports
-- * Pass root directory for imports resolution.
parseDhallExpression :: Text -> IO (Dhall.Expr Dhall.Src Dhall.X)
parseDhallExpression =
    either throwIO resolveImports . Dhall.exprFromText "(stdin)"
  where
    resolveImports
        :: Dhall.Expr Dhall.Src Dhall.Import
        -> IO (Dhall.Expr Dhall.Src Dhall.X)
    resolveImports expr =
        evalStateT (Dhall.loadWith expr) (Dhall.emptyStatus ".")

-- | Takes a Dhall @EXPRESSION@ and returns a following expression:
--
-- @
-- let
--     -- Type of 'Paths' data type so that it can be referred to in
--     -- EXPRESSION.
--     Paths = ...
--
-- in
--       λ(paths : Paths)
--     → EXPRESSION paths : TYPE
-- @
mkExpressionAndTypeCheck
    :: Maybe (Dhall.Expr Dhall.Src Dhall.X)
    -- ^ Expected type of the result.
    -> Dhall.Expr Dhall.Src Dhall.X
    -> Paths
    -> Either
        (Dhall.TypeError Dhall.Src Dhall.X)
        (Dhall.Expr Dhall.Src Dhall.X, Dhall.Expr Dhall.Src Dhall.X)
mkExpressionAndTypeCheck possiblyResultType bodyExpression paths =
    Dhall.typeOf expression <&> (Dhall.normalize expression, )
  where
    Dhall.InputType{..} = Dhall.inject @Paths

    bodyExpression' =
        maybe bodyExpression (Dhall.Annot bodyExpression) possiblyResultType

    expression =
        Dhall.Let "Paths" Nothing declared
            (Dhall.Lam "paths" declared bodyExpression')
        `Dhall.App` embed paths

throwLeft :: Exception e => Either e r -> IO r
throwLeft = either throwIO pure
