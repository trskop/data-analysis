{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude,
--               RecordWildCards, ScopedTypeVariables
--
-- TODO
module DataAnalysis.Sortable.Vector
    ( VectorSortAlgorithms(..)
    , Bounds(..)
    , AmericanFlagParams(..)
    , Unboxed(..)
    , sortBy
    )
  where

import Prelude (undefined)

import Data.Bool (Bool, otherwise)
import Data.Function (($))
import Data.Int (Int)
import Data.Ord (Ord((<)), Ordering)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)

import Data.Vector.Generic (Vector, modify)
import Data.Vector.Generic.Mutable (length)

import Data.Vector.Algorithms.AmericanFlag (Lexicographic)
import qualified Data.Vector.Algorithms.AmericanFlag as AmericanFlag
    ( Lexicographic(terminate, size, index)
    , sortBy
    )
import qualified Data.Vector.Algorithms.Heap as Heap
    ( partialSortBy
    , partialSortByBounds
    , selectBy
    , selectByBounds
    , sortBy
    , sortByBounds
    )
import qualified Data.Vector.Algorithms.Insertion as Insertion
    ( sortBy
    , sortByBounds
    , sortByBounds'
    )
import qualified Data.Vector.Algorithms.Intro as Intro
    ( partialSortBy
    , partialSortByBounds
    , selectBy
    , selectByBounds
    , sortBy
    , sortByBounds
    )
import qualified Data.Vector.Algorithms.Merge as Merge (sortBy)
import qualified Data.Vector.Algorithms.Tim as Tim (sortBy)

import Data.Default.Class (Default(def))


-- | Available algorithms for sorting vectors.
--
-- For details see
-- <https://hackage.haskell.org/package/vector-algorithms vector-algorithms>
-- package.
data VectorSortAlgorithms e
    = AmericanFlagSort !(AmericanFlagParams e)
    -- ^ This sort algorithm is suitable for sorting things in lexicografic
    -- order, like strings. When sorting elements that are instances of
    -- 'Lexicographic' type class, one can use:
    --
    -- @
    -- 'AmericanFlagSort' 'def'
    -- @
    --
    -- See <https://en.wikipedia.org/wiki/American_flag_sort Wikipedia: American flag sort>.

    | HeapSort !Bounds
    -- ^ Heap sort can be parametrized by lower and upper index of portion of a
    -- vector @[l, u)@ that should be sorted. To sort entire vector use:
    --
    -- @
    -- 'HeapSort' 'def'
    -- @
    --
    -- See <https://en.wikipedia.org/wiki/Heapsort Wikipedia: Heapsort>.

    | HeapSelectSort !Bounds !Int
    -- ^ Moves the lowest @k@ elements in the portion @[l, u)@, as specified by
    -- the 'Bounds', and moves them in to the front of the array, in no
    -- particular order. To select @k@ elements from entire vector use:
    --
    -- @
    -- 'HeapSelectSort' 'def' k
    -- @

    | HeapPartialSort !Bounds !Int
    -- ^ Moves the lowest @k@ element in the portion @[l, u)@, as specified by
    -- the 'Bounds', and moves them in to front of the vector, sorted. To sort
    -- lowest @k@ elements from entire vector use:
    --
    -- @
    -- 'HeapPartialSort' 'def' k
    -- @

    | InsertionSort !Bounds
    -- ^ A simple insertion sort algorithm. Though it's O(n^2), its iterative
    -- nature can be beneficial for small arrays. To sort the whole vector, use:
    --
    -- @
    -- 'InsertionSort' 'def'
    -- @

    | InsertionSortWithSortedPrefix !Bounds !Int
    -- ^ Sorts the portion of the array delimited by @[l, u)@ under the
    -- assumption that @[l, k)@ is already sorted. To sort the whole vector,
    -- use:
    --
    -- @
    -- 'InsertionSortWithSortedPrefix' 'def' k
    -- @

    | IntroSort !Bounds
    -- ^ A hybrid sorting algorithm that combines /quicksort/ and /heapsort/.
    -- To sort entire vector, use:
    --
    -- @
    -- 'IntroSort' 'def'
    -- @
    --
    -- See <https://en.wikipedia.org/wiki/Introsort Wikipedia: Introsort>

    | IntroSelectSort !Bounds !Int
    -- ^ Moves the lowest @k@ elements in the portion @[l, u)@, as specified by
    -- the 'Bounds', and moves them in to the front of the array, in no
    -- particular order. To select @k@ elements from entire vector use:
    --
    -- @
    -- 'IntroSelectSort' 'Bounds' k
    -- @

    | IntroPartialSort !Bounds !Int
    -- ^ Moves the lowest @k@ element in the portion @[l, u)@, as specified by
    -- the 'Bounds', and moves them in to front of the vector, sorted. To sort
    -- lowest @k@ elements from entire vector use:
    --
    -- @
    -- 'IntroPartialSort' 'def' k
    -- @

    | MergeSort
    -- ^ A simple top-down merge sort. The temporary buffer is preallocated to
    -- 1/2 the size of the input array, and shared through the entire sorting
    -- process to ease the amount of allocation performed in total. This is a
    -- stable sort.

--  | OptimalSort !OptimalSortParams    -- TODO
--  | RadixSort !(RadixSortParams e)    -- TODO

    | TimSort
    -- ^ Timsort is a complex, adaptive, bottom-up merge sort. It is designed
    -- to minimize comparisons as much as possible, even at some cost in
    -- overhead. Thus, it may not be ideal for sorting simple primitive types,
    -- for which comparison is cheap. It may, however, be significantly faster
    -- for sorting arrays of complex values (strings would be an example,
    -- though an algorithm not based on comparison would probably be superior
    -- in that particular case).
  deriving (Generic, Generic1, Typeable)

-- |
-- @
-- 'def' = 'IntroSort' ('def' :: 'Bounds')
-- @
instance Default (VectorSortAlgorithms e) where
    def = IntroSort def

-- | Parameters used by /American flag sort algorithm/.
data AmericanFlagParams e = AmericanFlagParams
    { isStripeComplete :: !(e -> Int -> Bool)
    , numberOfBuckets :: !Int
    , radixFunction :: !(Int -> e -> Int)
    }
  deriving (Generic, Typeable)

-- |
-- @
-- 'def' :: forall e. 'Lexicographic' e => 'AmericanFlagParams' e
-- 'def' = 'AmericanFlagParams'
--     { 'isStripeComplete' = 'AmericanFlag.terminate'
--     , 'numberOfBuckets' = 'AmericanFlag.size' ('undefined' :: e)
--       -- ^ Function 'AmericanFlag.size' doesn't use its argument. It's there
--       -- just to pass the type information and not a value.
--     , 'radixFunction' = 'AmericanFlag.index'
--     }
-- @
instance forall e. Lexicographic e => Default (AmericanFlagParams e) where
    def = AmericanFlagParams
        { isStripeComplete = AmericanFlag.terminate
        , numberOfBuckets = AmericanFlag.size (undefined :: e)
        , radixFunction = AmericanFlag.index
        }

-- | Interval of indices @[l, u)@ of vector elements to be sorted by a sorting
-- algorithm.
data Bounds = Bounds
    { lowerIndex :: !Int
    -- ^ Lower index @l@ of the portion to sort. Value @0@ is used if set to a
    -- negative value.
    , upperIndex :: !Int
    -- ^ Upper index @u@ of the portion to sort. Length of the vector is used
    -- if set to value that is @u < 0@.
    }
  deriving (Generic, Typeable)

-- |
-- @
-- 'def' = 'Bounds'
--     { 'lowerIndex' = 0   -- Sort the vector from beginning.
--     , 'upperIndex' = -1  -- Sort the vector up to its last element.
--     }
-- @
instance Default Bounds where
    def = Bounds
        { lowerIndex = 0
        , upperIndex = -1
        }

-- | Wrapper that provided all available sorting algorithms for vector types.
-- See 'VectorSortAlgorithms' for details.
sortBy
    :: Vector v a
    => (a -> a -> Ordering)
    -> VectorSortAlgorithms a
    -> v a
    -> v a
sortBy cmp params = modify $ \v -> case params of
    AmericanFlagSort AmericanFlagParams{..} ->
        AmericanFlag.sortBy cmp isStripeComplete numberOfBuckets radixFunction v

    HeapSort Bounds{lowerIndex = l, upperIndex = u}
      | l < 0, u < 0 -> Heap.sortBy cmp v
      | u < 0        -> Heap.sortByBounds cmp v l (length v)
      | l < 0        -> Heap.sortByBounds cmp v 0 u
      | otherwise    -> Heap.sortByBounds cmp v l u

    HeapSelectSort Bounds{lowerIndex = l, upperIndex = u} k
      | l < 0, u < 0 -> Heap.selectBy cmp v k
      | u < 0        -> Heap.selectByBounds cmp v l (length v) k
      | l < 0        -> Heap.selectByBounds cmp v 0 u k
      | otherwise    -> Heap.selectByBounds cmp v l u k

    HeapPartialSort Bounds{lowerIndex = l, upperIndex = u} k
      | l < 0, u < 0 -> Heap.partialSortBy cmp v k
      | u < 0        -> Heap.partialSortByBounds cmp v l (length v) k
      | l < 0        -> Heap.partialSortByBounds cmp v 0 u k
      | otherwise    -> Heap.partialSortByBounds cmp v l u k

    InsertionSort Bounds{lowerIndex = l, upperIndex = u}
      | l < 0, u < 0 -> Insertion.sortBy cmp v
      | u < 0        -> Insertion.sortByBounds cmp v l (length v)
      | l < 0        -> Insertion.sortByBounds cmp v 0 u
      | otherwise    -> Insertion.sortByBounds cmp v l u

    InsertionSortWithSortedPrefix Bounds{lowerIndex = l, upperIndex = u} k
      | l < 0, u < 0 -> Insertion.sortByBounds' cmp v 0 (length v) k
      | u < 0        -> Insertion.sortByBounds' cmp v l (length v) k
      | l < 0        -> Insertion.sortByBounds' cmp v 0 u k
      | otherwise    -> Insertion.sortByBounds' cmp v l u k

    IntroSort Bounds{lowerIndex = l, upperIndex = u}
      | l < 0, u < 0 -> Intro.sortBy cmp v
      | u < 0        -> Intro.sortByBounds cmp v l (length v)
      | l < 0        -> Intro.sortByBounds cmp v 0 u
      | otherwise    -> Intro.sortByBounds cmp v l u

    IntroSelectSort Bounds{lowerIndex = l, upperIndex = u} k
      | l < 0, u < 0 -> Intro.selectBy cmp v k
      | u < 0        -> Intro.selectByBounds cmp v l (length v) k
      | l < 0        -> Intro.selectByBounds cmp v 0 u k
      | otherwise    -> Intro.selectByBounds cmp v l u k

    IntroPartialSort Bounds{lowerIndex = l, upperIndex = u} k
      | l < 0, u < 0 -> Intro.partialSortBy cmp v k
      | u < 0        -> Intro.partialSortByBounds cmp v l (length v) k
      | l < 0        -> Intro.partialSortByBounds cmp v 0 u k
      | otherwise    -> Intro.partialSortByBounds cmp v l u k

    MergeSort -> Merge.sortBy cmp v
    TimSort -> Tim.sortBy cmp v

-- | Wrapper for unboxed vectors.
newtype Unboxed v a = Unboxed {getUnboxed :: v a}
  deriving (Generic, Generic1, Typeable)
