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
module DataAnalysis.Decode.Cassava
    ( CassavaDecodingException(..)
    , CassavaOptions(..)

    , cassavaEitherDecode
    , cassavaEitherDecodeNamed

    -- * Re-exported
    --
    -- | From cassava package.
    )
  where

import Control.Exception (Exception)
import Data.Bool (Bool(True))
import Data.Data (Data, Typeable)
import Data.Either (Either(Left, Right), either)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.String (String)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Show (Show)

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Vector (Vector)

import qualified Data.Csv as Cassava
    ( DecodeOptions(DecodeOptions, decDelimiter)
    , FromRecord
    , FromNamedRecord
    , HasHeader(HasHeader, NoHeader)
    , Header
    , decodeByNameWith
    , decodeWith
    , defaultDecodeOptions
    )
import Data.Default.Class (Default(def))


data CassavaDecodingException = CassavaDecodingException String
  deriving (Show, Typeable)

instance Exception CassavaDecodingException

data CassavaOptions = CassavaOptions
    { fieldDelimiter :: !Word8
    , inputHasHeader :: !Bool
    -- ^ Ignored when parsing named record fields.
    }
  deriving (Data, Eq, Generic, Typeable, Show)

-- |
-- @
-- 'def' = 'CassavaOptions'
--     { 'fieldDelimiter' = 44  -- comma (\',\')
--     , 'inputHasHeader' = 'True'
--     }
-- @
instance Default CassavaOptions where
    def = CassavaOptions
        { fieldDelimiter = Cassava.decDelimiter Cassava.defaultDecodeOptions
        , inputHasHeader = True
        }
    {-# INLINE def #-}

toCassavaDecodeOptions :: CassavaOptions -> Cassava.DecodeOptions
toCassavaDecodeOptions CassavaOptions{..} = Cassava.DecodeOptions
    { Cassava.decDelimiter = fieldDelimiter
    }
{-# INLINE toCassavaDecodeOptions #-}

cassavaEitherDecode
    :: Cassava.FromRecord a
    => CassavaOptions
    -> Lazy.ByteString
    -> Either CassavaDecodingException (Vector a)
cassavaEitherDecode opts@CassavaOptions{..} = castCassavaError
    . Cassava.decodeWith (toCassavaDecodeOptions opts) hasHeader
  where
    hasHeader = if inputHasHeader
        then Cassava.HasHeader
        else Cassava.NoHeader
{-# INLINE cassavaEitherDecode #-}

cassavaEitherDecodeNamed
    :: Cassava.FromNamedRecord a
    => CassavaOptions
    -> Lazy.ByteString
    -> Either CassavaDecodingException (Cassava.Header, Vector a)
cassavaEitherDecodeNamed opts =
    castCassavaError . Cassava.decodeByNameWith (toCassavaDecodeOptions opts)
{-# INLINE cassavaEitherDecodeNamed #-}

castCassavaError :: Either String a -> Either CassavaDecodingException a
castCassavaError = either (Left . CassavaDecodingException) Right
{-# INLINE castCassavaError #-}
