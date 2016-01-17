{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- TODO
module DataAnalysis.Encode
    ( module DataAnalysis.Encode.Class
    , module DataAnalysis.Encode.Aeson

    , encodeFile
    )
  where

import Data.Function ((.))
import System.IO (FilePath, IO)

import qualified Data.ByteString.Lazy as Lazy.ByteString (writeFile)

import DataAnalysis.Encode.Class
import DataAnalysis.Encode.Aeson hiding
    ( aesonEncodeToBuilderWith
    , aesonEncodeWith
    )


encodeFile
    :: Encode e a
    => proxy e
    -> (EncodingParams e)
    -> FilePath
    -> a
    -> IO ()
encodeFile proxy opts fileName =
     Lazy.ByteString.writeFile fileName . encodeWith proxy opts
