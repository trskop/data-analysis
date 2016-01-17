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
module DataAnalysis.Type
  where

import Data.Bool (Bool)
import Data.Ord (Ordering)


-- | Functions for comparison of two values.
type Comparator a = a -> a -> Ordering

type Equivalence a = a -> a -> Bool
