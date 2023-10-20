{- |
   Module      : Optics.Operators
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Module for utility optics operators
-}
module Optics.Operators
  ( (+~)
  , (<>~)
  )
where

import Optics.Core (A_Setter, Is, Optic, (%~))

(+~) :: (Num a, Is k A_Setter) => Optic k is s t a a -> a -> s -> t
v +~ y = v %~ (+ y)

(<>~) :: (Semigroup a, Is k A_Setter) => Optic k is s t a a -> a -> s -> t
v <>~ y = v %~ (<> y)
