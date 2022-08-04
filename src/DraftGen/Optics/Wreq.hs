{- |
   Module      : Optics.Wreq
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Reexports optics-core plus custom utility optics
-}
module Optics.Wreq (responseBody) where

import Network.Wreq (Response)
import Network.Wreq qualified as Wreq
import Optics.Lens (Lens, lensVL)

responseBody :: Lens (Response a) (Response b) a b
responseBody = lensVL Wreq.responseBody
