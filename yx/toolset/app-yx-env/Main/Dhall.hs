-- |
-- Module:      $Header$
-- Description: Utilities for working with Dhall.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Utilities for working with Dhall.
module Main.Dhall
    ( hPut
    )
  where

import System.IO (Handle, hPutStrLn)

import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import qualified Dhall (Inject, InputType(embed), inject)


hPut :: Dhall.Inject a => Handle -> a -> IO ()
hPut h a = do
    hPutDoc h . pretty $ Dhall.embed Dhall.inject a
    hPutStrLn h ""
