{-# LANGUAGE FlexibleContexts #-}
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
-- Portability:  FlexibleContexts, NoImplicitPrelude, TypeFamilies
--
-- TODO
module DataAnalysis.Groupable.Class
  where

import Data.Eq (Eq((==)))
import qualified Data.List as List (group, groupBy, nub, nubBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (group1, groupBy1, nub, nubBy)

import Data.Vector (Vector)
import qualified Data.Vector as Vector
    ( cons
    , empty
    , head
    , null
    , span
    )

import DataAnalysis.Type (Equivalence)


class Groupable c where
    {-# MINIMAL groupBy, nubBy #-}
    type GroupedValue c :: *
    type Container c :: * -> *

    groupBy
        :: (t ~ Container c)
        => Equivalence (GroupedValue c)
        -> c
        -> t c

    -- | Default implementation:
    --
    -- @
    -- 'group' = 'groupBy' ('==')
    -- @
    group
        :: (t ~ Container c, Eq (GroupedValue c))
        => c
        -> t c
    group = groupBy (==)

    nubBy :: Equivalence (GroupedValue c) -> c -> c

    -- | Default implementation:
    --
    -- @
    -- 'nub' = 'nubBy' ('==')
    -- @
    nub :: Eq (GroupedValue c) => c -> c
    nub = nubBy (==)

-- | Alias for 'nub'.
unique :: (Groupable c, Eq (GroupedValue c)) => c -> c
unique = nub

-- | Alias for 'nubBy'.
uniqueBy :: (Groupable c) => Equivalence (GroupedValue c) -> c -> c
uniqueBy = nubBy

instance Groupable [a] where
    type GroupedValue [a] = a
    type Container [a] = []

    groupBy = List.groupBy
    {-# INLINE groupBy #-}
    group = List.group
    {-# INLINE group #-}
    nubBy = List.nubBy
    {-# INLINE nubBy #-}
    nub = List.nub
    {-# INLINE nub #-}

instance Groupable (NonEmpty a) where
    type GroupedValue (NonEmpty a) = a
    type Container (NonEmpty a) = NonEmpty

    groupBy = NonEmpty.groupBy1
    {-# INLINE groupBy #-}
    group = NonEmpty.group1
    {-# INLINE group #-}
    nubBy = NonEmpty.nubBy
    {-# INLINE nubBy #-}
    nub = NonEmpty.nub
    {-# INLINE nub #-}

instance Groupable (Vector a) where
    type GroupedValue (Vector a) = a
    type Container (Vector a) = Vector

    -- These are just naive implementations of groupBy and nubBy; they need
    -- serious benchmarking and probably optimization, too.

    groupBy _  v | Vector.null v = Vector.empty
    groupBy eq v = xs `Vector.cons` groupBy eq rest
      where
        -- Vector.head can not fail, since we have checked if the vector is not
        -- empty.
        (xs, rest) = Vector.span (eq (Vector.head v)) v

    nubBy _  v | Vector.null v = v
    nubBy eq v = x `Vector.cons` nubBy eq rest
      where
        -- Vector.head can not fail, since we have checked if the vector is not
        -- empty.
        x = Vector.head v
        (_, rest) = Vector.span (eq x) v
