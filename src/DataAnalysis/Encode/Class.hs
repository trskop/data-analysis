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
module DataAnalysis.Encode.Class
    ( Encode(..)
    )
  where

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Builder as ByteString (Builder)

import qualified Data.Aeson as Aeson (ToJSON)
import Data.Default.Class (Default(def))

import DataAnalysis.Type.Encoding (JSON)
import DataAnalysis.Encode.Aeson
    ( AesonEncodingOptions
    , aesonEncodeToBuilderWith
    , aesonEncodeWith
    )


class Encode e a where
    type EncodingParams e

    {-# MINIMAL encodeWith, encodeToBuilderWith #-}

    encodeWith :: proxy e -> (EncodingParams e) -> a -> Lazy.ByteString

    encode :: (Default (EncodingParams e)) => proxy e -> a -> Lazy.ByteString
    encode proxy = encodeWith proxy def

    encodeToBuilderWith
        :: proxy e
        -> EncodingParams e
        -> a
        -> ByteString.Builder

    encodeToBuilder
        :: (Default (EncodingParams e))
        => proxy e
        -> a
        -> ByteString.Builder
    encodeToBuilder proxy = encodeToBuilderWith proxy def

instance Aeson.ToJSON a => Encode JSON a where
    type EncodingParams JSON = AesonEncodingOptions

    encodeWith _proxy = aesonEncodeWith
    encodeToBuilderWith _proxy = aesonEncodeToBuilderWith

--instance Cassava.ToRecord a => Encode CVS [a] where
--
--    type EncodingParams CVS = CassavaEncodingOptions
