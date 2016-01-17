{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  MultiParamTypeClasses, NoImplicitPrelude, TypeFamilies
--
-- TODO
module DataAnalysis.Decode
    ( module DataAnalysis.Decode.Class

    , module DataAnalysis.Decode.Aeson
    , module DataAnalysis.Decode.Cassava

    , decodeFile
    , decodeFileStrict
    )
  where

import Control.Monad ((>=>))
import System.IO (IO, FilePath)

import qualified Data.ByteString as Strict.ByteString
import qualified Data.ByteString.Lazy as Lazy.ByteString

import DataAnalysis.Decode.Aeson hiding
    ( aesonDecode
    , aesonDecodeStrict
    , aesonEitherDecode
    , aesonEitherDecodeStrict
    )
import DataAnalysis.Decode.Cassava hiding
    ( cassavaEitherDecode
    , cassavaEitherDecodeNamed
    )
import DataAnalysis.Decode.Class


decodeFile
    :: Decode e c
    => proxy e
    -> DecodingParams e
    -> FilePath
    -> IO c
decodeFile proxy params = Lazy.ByteString.readFile >=> decodeIO proxy params

decodeFileStrict
    :: Decode e c
    => proxy e
    -> DecodingParams e
    -> FilePath
    -> IO c
decodeFileStrict proxy params =
    Strict.ByteString.readFile >=> decodeStrictIO proxy params
