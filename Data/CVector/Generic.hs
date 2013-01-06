-- |
-- Module      : Data.CVector
-- Copyright   : (c) 2012-2013 Michal Terepeta
-- License     : BSD-style
--
-- Maintainer  : Michal Terepeta <michal.terepeta@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Wrapper around Data.Vector.Generic; its mutable version implements efficient
-- pushBack and popBack operations.
--

module Data.CVector.Generic ( module Data.CVector.GenericInternal ) where

import Data.CVector.GenericInternal ( CVector )
import Data.CVector.GenericInternal hiding ( CVector(..) )
