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
module DataAnalysis.Class.HasMaxSample
  where


class HasMaxSample t a | t -> a where
    maxSample :: t -> a
