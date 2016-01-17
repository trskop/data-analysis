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
    , module Data.Bool
    , module Data.Either
    , module Data.Eq
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor
    , module Data.Int
    , module Data.Maybe
    , module Data.Monoid
    , module Data.Ord
--  , module Data.Semigroup
    , module Data.Traversable
    , module Data.Word

    , module Data.Vector

    , module Control.Lens
    , module Data.Default.Class

    , module DataAnalysis.Decode
    , module DataAnalysis.Encode
    , module DataAnalysis.Groupable.Class
    , module DataAnalysis.Lens
    , module DataAnalysis.Sortable
    , module DataAnalysis.Type.Encoding
    )
  where

import Prelude (Bounded(..), Num(..), Integral(..))

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Either
import Data.Eq
import Data.Foldable
import Data.Function hiding (on)
import Data.Functor
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Ord
--import Data.Semigroup
import Data.Traversable
import Data.Word

import Data.Vector
    ( Vector
    , fromList
    )

import Control.Lens
--import Data.Csv hiding ((.=), index)
import Data.Default.Class

import DataAnalysis.Decode
import DataAnalysis.Encode
import DataAnalysis.Groupable.Class
import DataAnalysis.Lens
import DataAnalysis.Sortable
import DataAnalysis.Type.Encoding
