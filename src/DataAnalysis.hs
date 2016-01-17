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
module DataAnalysis
    (
      module Prelude

    , module Control.Applicative
    , module Control.Monad
    , module Data.Bifunctor
    , module Data.Bool
    , module Data.Char
    , module Data.Either
    , module Data.Eq
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor
    , module Data.Int
    , module Data.Maybe
    , module Data.Monoid
    , module Data.Ord
    , module Data.Ratio
--  , module Data.Semigroup
    , module Data.String
    , module Data.Traversable
    , module Data.Tuple
    , module Data.Word

    , module Data.HashMap.Lazy
    , module Data.HashSet
    , module Data.IntMap
    , module Data.IntSet
    , module Data.Map
    , module Data.Scientific
    , module Data.Sequence
    , module Data.Set
    , module Data.Vector

    , module Control.Lens
    , module Data.Default.Class
    , module Data.Monoid.Endo
    , module Data.Monoid.Endo.Fold

    , module DataAnalysis.Decode
    , module DataAnalysis.Encode
    , module DataAnalysis.Groupable.Class
    , module DataAnalysis.Lens
    , module DataAnalysis.Sortable
    , module DataAnalysis.Type
    , module DataAnalysis.Type.Encoding
    )
  where

import Prelude
    ( Bounded(..)
    , Enum(..)
    , Integral(..)
    , Num(..)
    , Integer
    , Float
    , Double
    )

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Eq
import Data.Foldable
import Data.Function hiding (on)
import Data.Functor
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio
--import Data.Semigroup
import Data.String
import Data.Traversable
import Data.Tuple
import Data.Word

import Data.HashMap.Lazy (HashMap)
    -- Data type is the same for stirct and lazy HashMap.
import Data.HashSet (HashSet)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Scientific
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Vector
    ( Vector
    , fromList
    )

import Control.Lens
--import Data.Csv hiding ((.=), index)
import Data.Default.Class
import Data.Default.Instances.Base ()
import Data.Default.Instances.Containers ()
import Data.Monoid.Endo
import Data.Monoid.Endo.Fold

import DataAnalysis.Decode
import DataAnalysis.Encode
import DataAnalysis.Groupable.Class
import DataAnalysis.Lens
import DataAnalysis.Sortable
import DataAnalysis.Type
import DataAnalysis.Type.Encoding
