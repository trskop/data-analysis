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
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude,
--               RecordWildCards
--
-- TODO
module DataAnalysis.Decode.Aeson
    ( AesonDecodingException(..)
    , AesonOptions(..)

    , aesonDecode
    , aesonDecodeStrict
    , aesonEitherDecode
    , aesonEitherDecodeStrict
    )
  where

import Control.Exception (Exception)
import Data.Bool (Bool(False), otherwise)
import Data.Data (Data, Typeable)
import Data.Either (Either(Left, Right), either)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Maybe (Maybe)
import Data.String (String)
import GHC.Generics (Generic)
import Text.Show (Show)

import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)

import qualified Data.Aeson as Aeson
    ( FromJSON
    , decode
    , decode'
    , decodeStrict
    , decodeStrict'
    , eitherDecode
    , eitherDecode'
    , eitherDecodeStrict
    , eitherDecodeStrict'
    )
import Data.Default.Class (Default(def))


data AesonDecodingException = AesonDecodingException String
  deriving (Data, Eq, Generic, Show, Typeable)

instance Exception AesonDecodingException

data AesonOptions = AesonOptions
    { useStrictParser :: Bool
    -- ^ Perform all value conversions immediately; this avoids building up
    -- thunks during parsing. Set this to 'Data.Bool.True' if most of the JSON
    -- data needs to be accessed.
    }
  deriving (Data, Generic, Show, Typeable)

-- |
-- @
-- 'def' = 'AesonOptions'
--     { 'useStrictParser' = 'False'
--     }
-- @
instance Default AesonOptions where
    def = AesonOptions
        { useStrictParser = False
        }
    {-# INLINE def #-}

aesonEitherDecode
    :: Aeson.FromJSON a
    => AesonOptions
    -> Lazy.ByteString
    -> Either AesonDecodingException a
aesonEitherDecode AesonOptions{..}
  | useStrictParser = castAesonError . Aeson.eitherDecode'
  | otherwise       = castAesonError . Aeson.eitherDecode

aesonDecode
    :: Aeson.FromJSON a
    => AesonOptions
    -> Lazy.ByteString
    -> Maybe a
aesonDecode AesonOptions{..}
  | useStrictParser = Aeson.decode'
  | otherwise       = Aeson.decode

aesonDecodeStrict
    :: Aeson.FromJSON a
    => AesonOptions
    -> Strict.ByteString
    -> Maybe a
aesonDecodeStrict AesonOptions{..}
  | useStrictParser = Aeson.decodeStrict'
  | otherwise       = Aeson.decodeStrict

aesonEitherDecodeStrict
    :: Aeson.FromJSON a
    => AesonOptions
    -> Strict.ByteString
    -> Either AesonDecodingException a
aesonEitherDecodeStrict AesonOptions{..}
  | useStrictParser = castAesonError . Aeson.eitherDecodeStrict'
  | otherwise       = castAesonError . Aeson.eitherDecodeStrict

castAesonError :: Either String a -> Either AesonDecodingException a
castAesonError = either (Left . AesonDecodingException) Right
{-# INLINE castAesonError #-}
