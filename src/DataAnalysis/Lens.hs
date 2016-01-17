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


-- | Perform binary function on results of two 'view' applications. This is an
-- equivalent of 'Data.Function.on' from "Data.Function" module; only
-- difference is that it tages 'Getting' instead of pure function @s -> a@.
--
-- To reconstruct 'Data.Function.on' from "Data.Function" module, just use 'to':
--
-- >>> ((+) `on` at id) 1 2
-- 3
--
-- Usage examples:
--
-- >>> ((==) `on` _1) ("hello",1) ("world",1 :: Int)
-- False
--
-- >>> ((==) `on` _2) ("hello",1) ("world",1 :: Int)
-- True
on :: (a -> a -> t) -> Getting a s a -> s -> s -> t
(f `on` l) s1 s2 = f (s1 ^. l) (s2 ^. l)
infixl 0 `on`

-- | Similar to 'on', but instead of '^.' operator it uses '^.', which performs
-- equivalent of \"safe head\".
preon :: (Maybe a -> Maybe a -> t) -> Getting (First a) s a -> s -> s -> t
(f `preon` l) s1 s2 = f (s1 ^? l) (s2 ^? l)
infixl 0 `preon`
