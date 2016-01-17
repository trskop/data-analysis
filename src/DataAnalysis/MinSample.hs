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
module DataAnalysis.MinSample
  where

import Prelude (Bounded(minBound))

import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Ord (Ord)
import qualified Data.Ord as Ord (Ord(min))
import Data.Semigroup (Semigroup((<>)))
import GHC.Generics (Generic, Generic1)
import Text.Show (Show)

import Data.Default.Class (Default(def))

import DataAnalysis.Class.FromSample (FromSample(fromSample))
import DataAnalysis.Class.HasMinSample (HasMinSample(minSample))


newtype MinSample a = MinSample a
  deriving (Bounded, Data, Eq, Generic, Generic1, Ord, Show, Typeable)

instance Bounded a => Default (MinSample a) where
    def = MinSample minBound

instance Ord a => Semigroup (MinSample a) where
    MinSample min1 <> MinSample min2 = MinSample (min1 `Ord.min` min2)

instance FromSample MinSample where
    fromSample = MinSample

instance HasMinSample (MinSample a) a where
    minSample (MinSample a) = a
