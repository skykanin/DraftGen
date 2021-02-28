{- |
   Module      : Util
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Module for utility functions
-}
module Util (fileName) where

import Control.Lens ((^.))
import Text.Printf
import Types (PackConfig, amount, set)

-- | Produce filename with set and pack amount information
fileName :: PackConfig -> String -> String
fileName cfg = printf "%d%s%s.json" (cfg ^. amount) (cfg ^. set)
