{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  FlexibleContexts, FlexibleInstances, NoImplicitPrelude,
--               TypeFamilies, UndecidableInstances
--
-- TODO
module DataAnalysis.Sortable.Class
    (
      Sortable(..)
    , sortUsing
    , sortBy
    , sort
    )
  where

import qualified Data.List as List (sortBy)
import Data.Ord (Ord(compare))

import Data.Vector (Vector)
import qualified Data.Vector.Generic as Generic (Vector)
import Data.Vector.Primitive (Prim)
import qualified Data.Vector.Primitive as Primitive (Vector)
import Data.Vector.Storable.Mutable (Storable)
import qualified Data.Vector.Storable as Storable (Vector)
import Data.Vector.Unboxed as Unboxed (Unbox)
import qualified Data.Vector.Unboxed as Unboxed (Vector)

import Data.Default.Class (Default(def))

import DataAnalysis.Sortable.Vector
    ( Unboxed(Unboxed)
    , VectorSortAlgorithms
    )
import qualified DataAnalysis.Sortable.Vector as Vector (sortBy)
import DataAnalysis.Type (Comparator)


-- | Containers that can be sorted.
class Sortable c where
    -- | Additional parameters required by the sorting function. E.g. selecting
    -- sort algorithm, etc.
    type SortParams c

    -- | Type family for container elements.
    type SortedValue c

    -- | Sort elements of a container using provided comparator. Sorting
    -- algorithm is selected and/or parametrized by @'SortParams' a@.
    sortByUsing
        :: Comparator (SortedValue c)
        -- ^ Function for comparison of container elements. In example
        -- 'compare' is a comparator for 'Ord' instances.
        -> SortParams c
        -- ^ Additional parameters required by the sorting function. E.g.
        -- selecting sort algorithm, etc.
        -> c
        -- ^ Unsorted container filled with entries of type @'SortedValue' c@.
        -> c
        -- ^ Sorted container filled with entries of type @'SortedValue' c@.

-- | Sort elements of a container using standard ordering defined by 'Ord'
-- instance for container elements @'SortedValue' c@.
sortUsing
    :: (Sortable c, Ord (SortedValue c))
    => SortParams c
    -- ^ Additional parameters required by the sorting function. E.g. selecting
    -- sort algorithm, etc.
    -> c
    -- ^ Unsorted container filled with entries of type @'SortedValue' c@.
    -> c
    -- ^ Sorted container filled with entries of type @'SortedValue' c@.
sortUsing = sortByUsing compare
{-# INLINE sortUsing #-}

-- | Sort elements of a container using provided 'Comparator'; and use
-- 'Default' sorting algorithm and parameters.
--
-- For standard Haskell list @[a]@ it behaves as a standard 'List.sortBy'
-- function from "Data.List".
sortBy
    :: (Sortable c, Default (SortParams c))
    => Comparator (SortedValue c)
    -- ^ Function for comparison of container elements. In example 'compare' is
    -- a comparator for 'Ord' instances.
    -> c
    -- ^ Unsorted container filled with entries of type @'SortedValue' c@.
    -> c
    -- ^ Sorted container filled with entries of type @'SortedValue' c@.
sortBy cmp = sortByUsing cmp def
{-# INLINE sortBy #-}

-- | Sort elements of a container using standard ordering defined by 'Ord'
-- instance for container elements @'SortedValue' c@; and use 'Default' sorting
-- algorithm and parameters.
--
-- For standard Haskell list @[a]@ it behaves as a standard 'Data.List.sort' function from "Data.List".
sort
    :: (Sortable c, Default (SortParams c), Ord (SortedValue c))
    => c
    -- ^ Unsorted container filled with entries of type @'SortedValue' c@.
    -> c
    -- ^ Sorted container filled with entries of type @'SortedValue' c@.
sort = sortByUsing compare def
{-# INLINE sort #-}

-- | Function 'sortByUsing' defined using standard 'List.sortBy' from
-- "Data.List" module.
instance Sortable [a] where
    type SortParams [a] = ()
    type SortedValue [a] = a

    sortByUsing cmp () = List.sortBy cmp
    {-# INLINE sortByUsing #-}

instance Generic.Vector Vector a => Sortable (Vector a) where
    type SortParams (Vector a) = VectorSortAlgorithms a
    type SortedValue (Vector a) = a

    sortByUsing = Vector.sortBy
    {-# INLINE sortByUsing #-}

instance
    (Prim a, Generic.Vector Primitive.Vector a)
    => Sortable (Primitive.Vector a)
  where
    type SortParams (Primitive.Vector a) = VectorSortAlgorithms a
    type SortedValue (Primitive.Vector a) = a

    sortByUsing = Vector.sortBy
    {-# INLINE sortByUsing #-}

instance
    (Storable a, Generic.Vector Storable.Vector a)
    => Sortable (Storable.Vector a)
  where
    type SortParams (Storable.Vector a) = VectorSortAlgorithms a
    type SortedValue (Storable.Vector a) = a

    sortByUsing = Vector.sortBy
    {-# INLINE sortByUsing #-}

instance
    (Unbox a, Generic.Vector v a, v ~ Unboxed.Vector)
    => Sortable (Unboxed v a)
  where
    type SortParams (Unboxed v a) = VectorSortAlgorithms a
    type SortedValue (Unboxed v a) = a

    sortByUsing cmp params (Unboxed v) = Unboxed (Vector.sortBy cmp params v)
    {-# INLINE sortByUsing #-}
