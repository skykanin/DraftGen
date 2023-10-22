{- |
   Module      : Prelude
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Slightly modified Prelude with some additions.
-}
module Prelude
  ( module Control.Applicative
  , module Control.Monad
  , module Data.Either
  , module Data.Foldable
  , module Data.Traversable
  , module Data.Functor
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module P
  , module Optics.Core
  , List
  , HasCallStack
  , (...)
  , expectJust
  , fromJust
  , identity
  , unexpectedError
  )
where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Foldable
import Data.Functor
import Data.List hiding (uncons)
import Data.Maybe hiding (fromJust)
import Data.Monoid
import Data.Traversable
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Optics.Core
import "base" Prelude hiding (id)
import "base" Prelude qualified as P

-- | Prefix list name
type List = []

-- | Compose two functions. @f ... g@ is similar to @f . g@ except that g will be fed /two/ arguments instead of one before handing its result to @f@.
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

-- | Replacement for 'id' that doesn't collide with "identifier".
identity :: a -> a
identity = P.id

-- | General version of 'fromJust' with a custom error message
expectJust :: HasCallStack => String -> Maybe a -> a
expectJust msg = \case
  P.Nothing -> unexpectedError msg
  P.Just a -> a

-- | Replacement for 'Data.Maybe.fromJust' that provides useful
-- information on failure.
fromJust :: HasCallStack => Maybe a -> a
fromJust = expectJust "fromJust received Nothing"

-- | Like 'error', but with a more conspicous name.
unexpectedError :: HasCallStack => String -> a
unexpectedError str = withFrozenCallStack $ error str
