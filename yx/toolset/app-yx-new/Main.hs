module Main (main)
  where

import Control.Applicative (optional)
import Control.Monad (unless, when)
import Data.Foldable (asum, forM_)
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import System.IO (stderr)

import CommandWrapper.Prelude
    ( Params(Params, config)
    , dieWith
    , subcommandParams
    )
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.IO as Text (putStrLn, writeFile)
import qualified Dhall
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
  deriving anyclass (Dhall.Interpret)

type Template = [FileTemplate]

data NamedTemplates = NamedTemplates
    { name :: Text
    , template :: Template
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

newtype Config = Config
    { templates :: [NamedTemplates]
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Interpret)

data Mode a
    = New Text (Maybe FilePath) a
    | List a
    | Help a
  deriving stock (Generic, Show)

main :: IO ()
main = do
    params@Params{config = configFile} <- subcommandParams
    mode <- Turtle.options "TODO: Describe me!" parseOptions
    config <- Dhall.inputFile Dhall.auto configFile
    realMain params config mode

parseOptions :: Turtle.Parser (Mode (Config -> Config))
parseOptions = asum
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
        dieWith params stderr 125 "Bug: This should not happen at the moment."

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
