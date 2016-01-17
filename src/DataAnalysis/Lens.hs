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
module DataAnalysis.Lens
  where

import Data.Maybe (Maybe)
import Data.Monoid (First)

import Control.Lens


-- |
--
-- To reconstruct 'Data.Function.on' from "Data.Function" module, just use 'to':
--
-- >>> ((+) `on` at id) 1 2
-- 3
on :: (a -> a -> t) -> Getting a s a -> s -> s -> t
(f `on` l) s1 s2 = f (s1 ^. l) (s2 ^. l)

preon :: (Maybe a -> Maybe a -> t) -> Getting (First a) s a -> s -> s -> t
(f `preon` l) s1 s2 = f (s1 ^? l) (s2 ^? l)
