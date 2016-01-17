{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               MultiParamTypeClasses, NoImplicitPrelude
--
-- TODO
module DataAnalysis.MaxSample
  where

import Prelude (Bounded(maxBound))

import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Ord (Ord)
import qualified Data.Ord as Ord (Ord(max))
import Data.Semigroup (Semigroup((<>)))
import GHC.Generics (Generic, Generic1)
import Text.Show (Show)

import Data.Default.Class (Default(def))

import DataAnalysis.Class.FromSample (FromSample(fromSample))
import DataAnalysis.Class.HasMaxSample (HasMaxSample(maxSample))


newtype MaxSample a = MaxSample a
  deriving (Bounded, Data, Eq, Generic, Generic1, Ord, Show, Typeable)

instance Bounded a => Default (MaxSample a) where
    def = MaxSample maxBound

instance Ord a => Semigroup (MaxSample a) where
    MaxSample max1 <> MaxSample max2 = MaxSample (max1 `Ord.max` max2)

instance FromSample MaxSample where
    fromSample = MaxSample

instance HasMaxSample (MaxSample a) a where
    maxSample (MaxSample a) = a
