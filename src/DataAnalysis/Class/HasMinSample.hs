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
-- Portability:  FunctionalDependencies, MultiParamTypeClasses,
--               NoImplicitPrelude
--
-- TODO
module DataAnalysis.Class.HasMinSample
  where


class HasMinSample t a | t -> a where
    minSample :: t -> a
