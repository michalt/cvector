-- |
-- Module      : Data.CVector
-- Copyright   : (c) 2012-2013 Michal Terepeta
-- License     : BSD-style
--
-- Maintainer  : Michal Terepeta <michal.terepeta@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Wrapper around Data.Vector.Generic.Mutable implementing efficient pushBack
-- and popBack operations.
--

module Data.CVector.Generic.Mutable
  ( module Data.CVector.Generic.MutableInternal ) where

import Data.CVector.Generic.MutableInternal ( CMVector )
import Data.CVector.Generic.MutableInternal hiding ( CMVector(..) )
