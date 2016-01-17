{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
--               FunctionalDependencies, MultiParamTypeClasses,
--               NoImplicitPrelude
--
-- TODO
module DataAnalysis.Range
  where

import Prelude (Bounded(maxBound, minBound), Num((-)))

import Data.Data (Data, Typeable)
import Data.Eq (Eq((==)))
import Data.Function (($), (.), on)
import Data.Foldable (Foldable(foldMap))
import Data.Maybe (Maybe(Just))
import Data.Ord (Ord{-(compare)-})
import Data.Semigroup (Option(Option), Semigroup((<>)))
import GHC.Generics (Generic, Generic1)
import Text.Show (Show{-(showsPrec)-})

import Data.Default.Class (Default(def))

import DataAnalysis.Class.FromSample (FromSample(fromSample))
import DataAnalysis.Class.HasMaxSample (HasMaxSample(maxSample))
import DataAnalysis.Class.HasMinSample (HasMinSample(minSample))
import DataAnalysis.MaxSample (MaxSample(MaxSample))
import DataAnalysis.MinSample (MinSample(MinSample))


-- | Computes:
--
-- * Minimum sample value.
-- * Maximum sample value.
-- * Range, i.e. difference between maximum and minimum sample values.
data Range a = Range !(MinSample a) !(MaxSample a)
  deriving (Data, Generic, Generic1, Show, Typeable)

instance HasMinSample (Range a) a where
    minSample (Range (MinSample a) _) = a

instance HasMaxSample (Range a) a where
    maxSample (Range _ (MaxSample a)) = a

-- | @('==') = ('==') ``on`` 'getRange'@
instance (Num a, Eq a) => Eq (Range a) where
    (==) = (==) `on` getRange

instance FromSample Range where
    fromSample n = Range (MinSample n) (MaxSample n)

instance Ord a => Semigroup (Range a) where
    Range min1 max1 <> Range min2 max2 =
        Range (min1 <> min2) (max1 <> max2)

instance Bounded a => Default (Range a) where
    def = Range minBound maxBound

getRangeWith :: Num b => (a -> b) -> Range a -> b
getRangeWith f (Range (MinSample min) (MaxSample max)) = f max - f min
{-# INLINE getRangeWith #-}

getRange :: Num a => Range a -> a
getRange (Range (MinSample min) (MaxSample max)) = max - min
{-# INLINE getRange #-}

sample :: Ord a => a -> Range a
sample a = Range (MinSample a) (MaxSample a)

range :: (Ord a, Foldable f) => f a -> Option (Range a)
range = foldMap $ Option . Just . sample
