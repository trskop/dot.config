{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Main.Config.Preferences
-- Description: Cache of user preferences.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Cache of user preferences.
module Main.Config.Preferences
    ( Preferences(..)
    , KnownFile(..)
    , Status(..)
    , read
    , modify
    )
  where

import Control.Exception (bracket)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor ((<$), (<$>))
import Data.String (fromString)
import Data.Tuple (snd)
import GHC.Generics (Generic)
import System.IO (FilePath, IO, hClose, hPutStrLn, openTempFile)
import Text.Show (Show)

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import qualified Dhall
    ( Encoder(embed)
    , FromDhall
    , ToDhall
    , auto
    , inject
    , inputFile
    )
import System.Directory (renameFile)
import System.FilePath (splitFileName, takeDirectory)
import qualified Turtle (mktree, testfile)


data Status
    = Allowed
    | Ignored
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

data KnownFile = KnownFile
    { file :: Text
    , status :: Status
--  , timetamp :: Timestamp -- TODO: Consider adding this.
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

-- | User preferences are a cache of user decisions.  It is considered safe to
-- get rid of it.
newtype Preferences = Preferences
    { knownFiles :: [KnownFile]
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

empty :: Preferences
empty = Preferences
    { knownFiles = []
    }

read :: FilePath -> IO Preferences
read file = do
    configExists <- Turtle.testfile (fromString file)
    if configExists
        then Dhall.inputFile Dhall.auto file

        else
            let dir = fromString (takeDirectory file)
            in  -- Make sure that we have a directory to write preferences to.
                -- Since we have only read and modify operations it is safe to
                -- do it only here.
                empty <$ Turtle.mktree dir

modify :: FilePath -> (Preferences -> Preferences) -> IO ()
modify file f = do
    preferences <- f <$> read file

    temp <- withTempFile $ \(fp, h) ->
        fp <$ hPutDhall h preferences

    -- This operation is atomic.  We rename the temp file to the same name as
    -- preferences file, which will override it.
    renameFile temp file
  where
    hPutDhall h value = do
        hPutDoc h . pretty $ Dhall.embed Dhall.inject value
        hPutStrLn h ""

    withTempFile =
        let (dir, fileName) = splitFileName file
        in  -- Opening the temporary file in the same directory as the target
            -- makes sure that we are using the same file system. Therefore,
            -- 'renameFile' operation will be atomic.
            openTempFile dir fileName `bracket` (hClose . snd)
