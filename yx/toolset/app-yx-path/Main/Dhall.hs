-- |
-- Module:      Main.Dhall
-- Description: Generate Dhall configuration file with XDG Directories.
-- Copyright:   (c) 2018-2019 Peter Trško
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
import Control.Monad ((>=>))
import Data.Foldable (asum, traverse_)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Traversable (for)
import Data.Void (Void)
import Numeric.Natural (Natural)

import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Except (liftEither, runExcept, withExcept)
import Data.Either.Validation (validationToEither)
import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn, readFile)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import qualified Dhall
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
    , Expr
        ( Annot
        , App
        , Integer
        , Let
        , List
        , Natural
        , Optional
        , Record
        , RecordLit
        , Text
        )
    , Import
    , normalize
    , throws
    )
import qualified Dhall.Import as Dhall (emptyStatus, loadWith)
import qualified Dhall.Map (fromList)
import qualified Dhall.Parser as Dhall (Src, exprFromText)
import qualified Dhall.TypeCheck as Dhall (TypeError, typeOf)
import System.FilePath (takeDirectory)
import System.Exit (die)

import Main.Paths (Paths)


data Output = Plain | DhallExpression | DhallType

newtype Options = Options
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
    -> Dhall.Expr Dhall.Src Void
    -> Maybe (Dhall.Expr Dhall.Src Void)
    ->  ( (Dhall.Expr Dhall.Src Void, Dhall.Expr Dhall.Src Void)
        -> Dhall.Expr Dhall.Src Void
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
    -> Dhall.Expr Dhall.Src Void
    -> Maybe (Dhall.Expr Dhall.Src Void)
    -> IO ()
printPlain paths expression possiblyConfigExpression =
    either dieWithNotPlainType id result
  where
    result :: Either [Dhall.TypeError Dhall.Src Void] (IO ())
    result = runExcept $ asum
        [ printText <$> mkExpression Dhall.Text
        , printOptionalText <$> mkExpression (optional Dhall.Text)
        , printNatural <$> mkExpression Dhall.Natural
        , printInteger <$> mkExpression Dhall.Integer
        , printTextList <$> mkExpression (listOf Dhall.Text)
        , printNaturalList <$> mkExpression (listOf Dhall.Natural)
        , printIntegerList <$> mkExpression (listOf Dhall.Integer)
        ]

    listOf = (Dhall.List `Dhall.App`)
    optional = (Dhall.Optional `Dhall.App`)

    configExpression = fromMaybe emptyRecord possiblyConfigExpression

    mkExpression t = withExcept pure . liftEither
        $ fst <$> mkExpressionAndTypeCheck (Just t) expression configExpression paths

    dieWithNotPlainType _ = die
        "Error: Expected Text, Optional Text, Natural, Integer, or List of\
        \ them as result."

--  dieWithUnexpectedType t =
--      die ("Error: Expected " <> t <> " as result.")

    Dhall.Decoder{extract = extractText} = Dhall.auto @Text
    Dhall.Decoder{extract = extractMaybeText} = Dhall.auto @(Maybe Text)
    Dhall.Decoder{extract = extractNatural} = Dhall.auto @Natural
    Dhall.Decoder{extract = extractInteger} = Dhall.auto @Integer
    Dhall.Decoder{extract = extractTextList} = Dhall.auto @[Text]
    Dhall.Decoder{extract = extractNaturalList} = Dhall.auto @[Natural]
    Dhall.Decoder{extract = extractIntegerList} = Dhall.auto @[Integer]

    printText, printOptionalText, printNatural, printInteger
        :: Dhall.Expr Dhall.Src Void
        -> IO ()

    printText =
        (throws . extractText) >=> Text.putStrLn

    printOptionalText =
        (throws . extractMaybeText) >=> traverse_ Text.putStrLn

    printNatural =
        (throws . extractNatural) >=> (Text.putStrLn . showing)

    printInteger =
        (throws . extractInteger) >=> (Text.putStrLn . showing)

    printTextList =
        (throws . extractTextList) >=> mapM_ Text.putStrLn

    printNaturalList =
        (throws . extractNaturalList) >=> mapM_ (Text.putStrLn . showing)

    printIntegerList =
        (throws . extractIntegerList) >=> mapM_ (Text.putStrLn . showing)

    showing :: Show a => a -> Text
    showing = fromString . show

    throws = Dhall.throws . validationToEither

-- | Parse Dhall expression and resolve imports.
--
-- TODO:
--
-- * Support parsing a file
-- * Allow disabling imports
-- * Pass root directory for imports resolution.
parseDhallExpression :: Maybe FilePath -> Text -> IO (Dhall.Expr Dhall.Src Void)
parseDhallExpression possiblySourcePath =
    either throwIO resolveImports . Dhall.exprFromText sourcePath
  where
    sourcePath = fromMaybe "(command-line)" possiblySourcePath
    sourceDir = maybe "." takeDirectory possiblySourcePath

    resolveImports
        :: Dhall.Expr Dhall.Src Dhall.Import
        -> IO (Dhall.Expr Dhall.Src Void)
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
    :: Maybe (Dhall.Expr Dhall.Src Void)
    -- ^ Expected type of the result.
    -> Dhall.Expr Dhall.Src Void
    -- ^ Expression from command line.
    -> Dhall.Expr Dhall.Src Void
    -- ^ Parsed configuration file.
    -> Paths
    -> Either
        (Dhall.TypeError Dhall.Src Void)
        (Dhall.Expr Dhall.Src Void, Dhall.Expr Dhall.Src Void)
mkExpressionAndTypeCheck possiblyResultType bodyExpression configExpression paths = do
    expression <- mkExpression <$> Dhall.typeOf configExpression
    Dhall.typeOf expression <&> (Dhall.normalize expression, )
  where
    Dhall.Encoder{..} = Dhall.inject @Paths

    bodyExpression' =
        maybe bodyExpression (Dhall.Annot bodyExpression) possiblyResultType

    mkExpression configExpressionType =
        Dhall.Binding
            { Dhall.variable = "Paths"
            , Dhall.annotation = Nothing
            , Dhall.value = declared
            , Dhall.bindingSrc0 = Nothing
            , Dhall.bindingSrc1 = Nothing
            , Dhall.bindingSrc2 = Nothing
            }
        `let_` Dhall.Binding
            { Dhall.variable = "Config"
            , Dhall.annotation = Nothing
            , Dhall.value = configExpressionType
            , Dhall.bindingSrc0 = Nothing
            , Dhall.bindingSrc1 = Nothing
            , Dhall.bindingSrc2 = Nothing
            }
        `let_` Dhall.Binding
            { Dhall.variable = "config"
            , Dhall.annotation = Just (Nothing, configExpressionType)
            , Dhall.value = configExpression
            , Dhall.bindingSrc0 = Nothing
            , Dhall.bindingSrc1 = Nothing
            , Dhall.bindingSrc2 = Nothing
            }
        `let_` Dhall.Binding
            { Dhall.variable = "paths"
            , Dhall.annotation = Just (Nothing, declared)
            , Dhall.value = embed paths
            , Dhall.bindingSrc0 = Nothing
            , Dhall.bindingSrc1 = Nothing
            , Dhall.bindingSrc2 = Nothing
            }
        `let_` Dhall.Binding
            { Dhall.variable = "data"
            , Dhall.annotation = Just
                ( Nothing
                , recordType
                    [ ("paths", declared)
                    , ("config", configExpressionType)
                    ]
                )
            , Dhall.value = record
                [ ("paths", embed paths)
                , ("config", configExpression)
                ]
            , Dhall.bindingSrc0 = Nothing
            , Dhall.bindingSrc1 = Nothing
            , Dhall.bindingSrc2 = Nothing
            }
        `let_`
            bodyExpression'

    let_ = Dhall.Let
    infixr 9 `let_`

throwLeft :: Exception e => Either e r -> IO r
throwLeft = either throwIO pure

emptyRecord :: Dhall.Expr s a
emptyRecord = Dhall.RecordLit mempty

record :: [(Text, Dhall.Expr s a)] -> Dhall.Expr s a
record = Dhall.RecordLit . Dhall.Map.fromList

recordType :: [(Text, Dhall.Expr s a)] -> Dhall.Expr s a
recordType = Dhall.Record . Dhall.Map.fromList
