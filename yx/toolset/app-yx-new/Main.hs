-- |
-- Module:      Main
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main (main)
  where

import Control.Applicative (optional)
import Control.Monad (unless, when)
import Data.Foldable (asum, forM_)
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import System.IO (stderr, stdout)

import CommandWrapper.Subcommand.Prelude
    ( HaveCompletionInfo(completionInfoMode)
    , Params(Params, config)
    , completionInfoFlag
    , dieWith
    , printOptparseCompletionInfoExpression
    , subcommandParams
    )
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import Data.Text (Text)
import qualified Data.Text as Text (null, unpack)
import qualified Data.Text.IO as Text (putStrLn, writeFile)
import Dhall (FromDhall)
import qualified Dhall (auto, input)
import qualified Options.Applicative as Options
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , getPermissions
    , setCurrentDirectory
    , setPermissions
    )
import qualified System.Directory as Directory (Permissions(executable))
import System.FilePath (isRelative, takeDirectory)
import qualified Turtle


data FileTemplate = FileTemplate
    { filePath :: Text
    , content :: Text
    , executable :: Bool
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

type Template = [FileTemplate]

data NamedTemplates = NamedTemplates
    { name :: Text
    , template :: Template
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

newtype Config = Config
    { templates :: [NamedTemplates]
    }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall)

data Mode a
    = New Text (Maybe FilePath) a
    | List a
    | Help a
    | CompletionInfo
    | Completion Word [String]
  deriving stock (Generic, Show)

instance HaveCompletionInfo (Mode a) where
    completionInfoMode = const CompletionInfo

main :: IO ()
main = do
    params@Params{config = configExpr} <- subcommandParams
    mode <- Turtle.options description parseOptions
    config <- if Text.null configExpr
        then
            dieWith params stderr 1
                "Configuration file is required and it's missing."
        else
            Dhall.input Dhall.auto configExpr

    realMain params config mode
  where
    description = "Create a package/project structure from a template."

parseOptions :: Turtle.Parser (Mode (Config -> Config))
parseOptions = completionInfoFlag <*> asum
    [ new
        <$> Turtle.argText "TEMPLATE_NAME" "Name of template to use"
        <*> optional (Turtle.argText "DIRECTORY" "Directory")

    , Options.flag' (List id) $ mconcat
        [ Options.long "list"
        , Options.long "ls"
        , Options.short 'l'
        , Options.help "List templates"
        ]
    , pure (Help id)
    ]
  where
    new name dir = New name (Text.unpack <$> dir) id

realMain :: Params -> Config -> Mode (Config -> Config) -> IO ()
realMain params config = \case
    Help _ ->
        notYetImplemented

    CompletionInfo ->
        printOptparseCompletionInfoExpression stdout

    Completion _ _ ->
        notYetImplemented

    List f ->
        mapM_ Text.putStrLn $ templateNames (f config)

    New name possiblyDir f -> do
        forM_ possiblyDir $ \dir -> do
            directoryExists <- doesDirectoryExist dir
            unless directoryExists (createDirectoryIfMissing True dir)
            setCurrentDirectory dir

        createProjectFromTemplate params name
            $ name `HashMap.lookup` namedTemplates (f config)
  where
    namedTemplates Config{templates} =
        HashMap.fromList $ templates <&> \NamedTemplates{name, template} ->
            (name, template)

    templateNames Config{templates} = name <$> templates

    notYetImplemented =
        dieWith params stderr 125 "Bug: This should not happen at the moment."

createProjectFromTemplate :: Params -> Text -> Maybe Template -> IO ()
createProjectFromTemplate params name = \case
    Nothing ->
        dieWith params stderr 3 ("\"" <> name <> "\": No such template")

    Just files -> do
        -- We need to check that file paths are relative before attempting to
        -- create anything.
        forM_ files $ \FileTemplate{filePath} ->
            unless (isRelative $ Text.unpack filePath)
                . dieWith params stderr 1
                    $ "\"" <> name <> "\": \"" <> filePath
                    <> "\": File path must be relative."

        forM_ files $ \FileTemplate{filePath, content, executable} -> do
            let file = Text.unpack filePath
                dir = takeDirectory file

            directoryExists <- doesDirectoryExist dir
            unless directoryExists (createDirectoryIfMissing True dir)

            Text.writeFile file content
            when executable $ do
                perms <- getPermissions file
                setPermissions file perms{Directory.executable = True}
