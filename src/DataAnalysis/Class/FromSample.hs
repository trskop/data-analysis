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
module DataAnalysis.Class.FromSample
  where

import Data.Function ((.))
import Data.Functor.Identity (Identity(Identity))
import Data.Maybe (Maybe(Just))
import Data.Semigroup (Option(Option))


class FromSample t where
    fromSample :: a -> t a

instance FromSample Identity where
    fromSample = Identity
    {-# INLINE fromSample #-}

instance FromSample Maybe where
    fromSample = Just
    {-# INLINE fromSample #-}

instance FromSample Option where
    fromSample = Option . Just
    {-# INLINE fromSample #-}
