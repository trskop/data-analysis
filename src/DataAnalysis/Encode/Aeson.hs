{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  CPP, DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude,
--               RecordWildCards
--
-- TODO
module DataAnalysis.Encode.Aeson
    ( AesonEncodingOptions(..)

    , aesonEncodeWith
    , aesonEncodeToBuilderWith
    )
  where

import Data.Bool (Bool(False), otherwise)
import Data.Function ((.))
import Data.Int (Int)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as ByteString.Builder (lazyByteString)
import Data.Text (Text)

import qualified Data.Aeson as Aeson
    ( ToJSON
#if !MIN_VERSION_aeson(0,10,0)
        ( toJSON
#else
        ( toEncoding
        )
    , fromEncoding
#endif
    , encode
    )
#if !MIN_VERSION_aeson(0,10,0)
import qualified Data.Aeson.Encode as Aeson (encodeToBuilder)
#endif
import qualified Data.Aeson.Encode.Pretty as AesonPretty
    ( Config
        ( Config
        , confIndent
        , confCompare
        )
    , defConfig
    , encodePretty'
    )

import Data.Default.Class (Default(def))
import DataAnalysis.Type (Comparator)


data AesonEncodingOptions = AesonEncodingOptions
    { prettyPrint :: !Bool
    -- ^ 'False' means no peretty printing, and 'True' means pretty print using
    -- 'prettyPrintIndent' and 'prettyPrintCompareKeys'. Don't use pretty
    -- printing if raw speed is required.
    , prettyPrintIndent :: !Int
    -- ^ Number of spaces used for indentation per level of nesting.
    , prettyPrintCompareKeys :: !(Comparator Text)
    -- ^ Comparator used for sorting keys in objects.
    }
  deriving (Generic, Typeable)

-- |
-- @
-- 'def' = 'AesonEncodingOptions'
--     { 'prettyPrint' = 'False'
--     , 'prettyPrintIndent' = 4
--     , 'prettyPrintCompareKeys' = 'mempty'  -- = \\_ _ -> 'Data.Ord.EQ'
--     }
-- @
instance Default AesonEncodingOptions where
    def = AesonEncodingOptions
        { prettyPrint = False
        , prettyPrintIndent = AesonPretty.confIndent AesonPretty.defConfig
        , prettyPrintCompareKeys =
            AesonPretty.confCompare AesonPretty.defConfig
        }
    {-# INLINE def #-}

toAesonPrettyConfig :: AesonEncodingOptions -> AesonPretty.Config
toAesonPrettyConfig AesonEncodingOptions{..} = AesonPretty.Config
    { confIndent = prettyPrintIndent
    , confCompare = prettyPrintCompareKeys
    }
{-# INLINE toAesonPrettyConfig #-}

aesonEncodeWith
    :: Aeson.ToJSON a
    => AesonEncodingOptions
    -> a
    -> Lazy.ByteString
aesonEncodeWith opts@AesonEncodingOptions{..}
  | prettyPrint = AesonPretty.encodePretty' (toAesonPrettyConfig opts)
  | otherwise   = Aeson.encode
{-# INLINE aesonEncodeWith #-}

aesonEncodeToBuilderWith
        :: Aeson.ToJSON a
        => AesonEncodingOptions
        -> a
        -> ByteString.Builder
aesonEncodeToBuilderWith opts@AesonEncodingOptions{..}
  | prettyPrint = aesonPrettyEncodeToBuilder (toAesonPrettyConfig opts)
  | otherwise =
#if MIN_VERSION_aeson(0,10,0)
    Aeson.fromEncoding . Aeson.toEncoding
#else
    Aeson.encodeToBuilder . Aeson.toJSON
#endif
{-# INLINE aesonEncodeToBuilderWith #-}

-- | Unfortunately
-- <https://hackage.haskell.org/package/aeson-pretty aeson-pretty>
-- package provides only function that encodes JSON in to
-- 'Data.Text.Text' builder. This function uses that internally and the result
-- is transcoded in to 'Lazy.ByteString' 'ByteString.Builder'.
aesonPrettyEncodeToBuilder
    :: Aeson.ToJSON a
    => AesonPretty.Config
    -> a
    -> ByteString.Builder
aesonPrettyEncodeToBuilder = (ByteString.Builder.lazyByteString .)
    . AesonPretty.encodePretty'
{-# INLINE aesonPrettyEncodeToBuilder #-}
