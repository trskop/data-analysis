{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- Portability:  FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
--               NoImplicitPrelude, TypeFamilies
--
-- TODO
module DataAnalysis.Decode.Class
    ( Decode(..)
    )
  where

import Control.Exception (Exception, throw)
import Control.Monad (Monad(return))
import Data.Either (Either, either)
import Data.Function ((.), const)
import Data.Maybe (Maybe(Nothing, Just))
import System.IO (IO)

import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString (fromStrict, toStrict)
import Data.Vector (Vector)

import Control.Monad.TaggedException.Unsafe (Throws, throwsOne)
import qualified Data.Aeson as Aeson (FromJSON)
import qualified Data.Csv as Cassava (FromRecord, FromNamedRecord, Header)

import DataAnalysis.Decode.Aeson
    ( AesonOptions
    , AesonDecodingException
    , aesonDecode
    , aesonDecodeStrict
    , aesonEitherDecode
    , aesonEitherDecodeStrict
    )
import DataAnalysis.Decode.Cassava
    ( CassavaOptions
    , CassavaDecodingException
    , cassavaEitherDecode
    , cassavaEitherDecodeNamed
    )
import DataAnalysis.Type.Encoding (CSV, JSON)


class Exception (DecodingException e) => Decode e a where
    type DecodingParams e
    type DecodingException e

    {-# MINIMAL eitherDecode | eitherDecodeStrict #-}

    -- {{{ Lazy.ByteString ----------------------------------------------------

    eitherDecode
        :: proxy e
        -> DecodingParams e
        -> Lazy.ByteString
        -> Either (DecodingException e) a
    eitherDecode proxy params =
        eitherDecodeStrict proxy params . Lazy.ByteString.toStrict

    decode :: proxy e -> DecodingParams e -> Lazy.ByteString -> Maybe a
    decode = ((either (const Nothing) Just .) .) . eitherDecode

    decodeIO :: proxy e -> DecodingParams e -> Lazy.ByteString -> IO a
    decodeIO = ((either throw return .) .) . eitherDecode

    witnessingDecodeIO
        :: proxy e
        -> DecodingParams e
        -> Lazy.ByteString
        -> Throws (DecodingException e) IO a
    witnessingDecodeIO = ((throwsOne .) .) . decodeIO

    -- }}} Lazy.ByteString ----------------------------------------------------
    -- {{{ Strict.ByteString --------------------------------------------------

    eitherDecodeStrict
        :: proxy e
        -> DecodingParams e
        -> Strict.ByteString
        -> Either (DecodingException e) a
    eitherDecodeStrict proxy params =
        eitherDecode proxy params . Lazy.ByteString.fromStrict

    decodeStrict :: proxy e -> DecodingParams e -> Strict.ByteString -> Maybe a
    decodeStrict = ((either (const Nothing) Just .) .) . eitherDecodeStrict

    decodeStrictIO :: proxy e -> DecodingParams e -> Strict.ByteString -> IO a
    decodeStrictIO = ((either throw return .) .) . eitherDecodeStrict

    witnessingDecodeStrictIO
        :: proxy e
        -> DecodingParams e
        -> Strict.ByteString
        -> Throws (DecodingException e) IO a
    witnessingDecodeStrictIO = ((throwsOne .) .) . decodeStrictIO

    -- }}} Strict.ByteString --------------------------------------------------

-- {{{ Aeson ------------------------------------------------------------------

instance Aeson.FromJSON a => Decode JSON a where
    type DecodingParams JSON = AesonOptions
    type DecodingException JSON = AesonDecodingException

    eitherDecode _proxy = aesonEitherDecode
    {-# INLINE eitherDecode #-}
    decode _proxy = aesonDecode
    {-# INLINE decode #-}
    decodeStrict _proxy = aesonDecodeStrict
    {-# INLINE decodeStrict #-}
    eitherDecodeStrict _proxy = aesonEitherDecodeStrict
    {-# INLINE eitherDecodeStrict #-}

-- }}} Aeson ------------------------------------------------------------------
-- {{{ Cassava ----------------------------------------------------------------

instance Cassava.FromRecord a => Decode CSV (Vector a) where
    type DecodingParams CSV = CassavaOptions
    type DecodingException CSV = CassavaDecodingException

    eitherDecode _proxy = cassavaEitherDecode

instance Cassava.FromNamedRecord a => Decode CSV (Cassava.Header, Vector a)
  where
    type DecodingParams CSV = CassavaOptions
    type DecodingException CSV = CassavaDecodingException

    eitherDecode _proxy = cassavaEitherDecodeNamed

-- }}} Cassava ----------------------------------------------------------------
