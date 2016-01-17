{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- TODO
module DataAnalysis.Mean
  where

import Prelude (Fractional((/)), Num((+)))

import Data.Eq (Eq((==)))
import Data.Ord (Ord(compare))
import Data.Function ((.), on)
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (Monoid(mempty, mappend))
import Text.Show (Show(showsPrec))

import Data.Default.Class (Default(def))


data Mean a = Mean !a !a

-- | @('==') = ('==') ``on`` 'getMean'@
instance (Fractional a, Eq a) => Eq (Mean a) where
    (==) = (==) `on` getMean

-- | @'compare' = 'compare' ``on`` 'getMean'@
instance (Fractional a, Ord a) => Ord (Mean a) where
    compare = compare `on` getMean

instance (Fractional a, Show a) => Show (Mean a) where
    showsPrec n = showsPrec n . getMean

instance Num a => Monoid (Mean a) where
    mempty = Mean 0 0

    Mean count1 sum1 `mappend` Mean count2 sum2 =
        Mean (count1 + count2) (sum1 + sum2)

instance Num a => Default (Mean a) where
    def = Mean 0 0

getMeanWith :: Fractional b => (a -> b) -> Mean a -> b
getMeanWith f (Mean count sum) = f sum / f count
{-# INLINE getMeanWith #-}

getMean :: Fractional a => Mean a -> a
getMean (Mean count sum) = sum / count
{-# INLINE getMean #-}

sample :: Num a => a -> Mean a
sample = Mean 1

mean :: (Num a, Foldable f) => f a -> Mean a
mean = foldMap sample
