{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- TODO
module DataAnalysis.Type.Encoding
    ( JSON
    , json

    , CSV
    , csv
    )
  where

import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)


data JSON
  deriving (Generic, Typeable)

json :: Proxy JSON
json = Proxy

data CSV
  deriving (Generic, Typeable)

csv :: Proxy CSV
csv = Proxy
