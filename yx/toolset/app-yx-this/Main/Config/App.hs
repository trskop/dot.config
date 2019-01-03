-- |
-- Module:      Main.Config.App
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main.Config.App
    ( Config(..)
    , Defaults(..)
    , UpdateWhat(..)
    , read
    , writeDef
    )
  where

import Prelude hiding (read)

import GHC.Generics (Generic)
import System.IO (IOMode(WriteMode), hPutStrLn, withFile)

import CommandWrapper.Environment (Params(Params, config))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import qualified Dhall
    ( Inject
    , InputType(embed)
    , Interpret
    , auto
    , inject
    , inputFile
    )
import System.Directory (doesFileExist)


data Config = Config
    { defaults :: Defaults

    , bootstrapPackages :: [Text]
    , purgePackages :: [Text]
    , packages :: [Text]
    , timezone :: Text

-- TODO:
--  , userAction :: Action
--  , packageRepositories :: PackageRepository
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

data Defaults = Defaults
    { update :: [UpdateWhat]
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

-- TODO: Move into a different module?
data UpdateWhat
    = UpdateSystem
    | InstallPackages
    | UpdateUserEnvironment
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

read :: Params -> IO (Maybe Config)
read Params{config} = do
    configExists <- doesFileExist config
    if configExists
        -- TODO: Catch exception and propagate it in Either.
        then Just <$> Dhall.inputFile Dhall.auto config
        else pure Nothing

writeDef :: Params -> IO ()
writeDef Params{config} =
    withFile config WriteMode $ \h -> do
        hPutDoc h . pretty $ Dhall.embed Dhall.inject def
        hPutStrLn h ""

def :: Config
def = Config
    { bootstrapPackages =
        -- Store /etc in version control.  Git here is for 'etckeeper', but to
        -- also mark it as manually installed.
        [ "etckeeper", "git"

        -- Essential for most of these tools to work reliably when used under
        -- non-root user.
        , "sudo"

        -- Most of this tooling is in Haskell.  Stack provides a way how to run
        -- Haskell scripts and istall rest of the tooling.
        , "haskell-stack"

        -- Access repositories via HTTPS.  Hopefully most Debian-like systems
        -- have this by default now.
        , "apt-transport-https"
        ]

    , purgePackages =
        -- Get rid of all the abominations.
        [ "nano"
        ]

    , packages =
        -- Be aware of possible issues on Debian, especially useful when
        -- running on testing/unstable.
        [ "apt-listbugs", "apt-listchanges"

        -- Use more advanced fallback editor.
        , "vim-nox", "vim-doc"
        ]

    , timezone = "Europe/London"

    , defaults = Defaults
        { update = []   -- Empty is the same as all actions.
        }
    }