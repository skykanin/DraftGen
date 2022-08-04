{- |
   Module      : Util
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Module for utility functions
-}
module Util (
  appName,
  cardCacheName,
  fileName,
  landName,
  packName,
  tokenName,
) where

import Optics.Core ((^.))
import Text.Printf (printf)
import Types (PackConfig)
import Types qualified

appName :: FilePath
appName = "DraftGen"

cardCacheName :: FilePath
cardCacheName = "CardData.json"

landName :: FilePath
landName = "lands"

packName :: FilePath
packName = "packs"

tokenName :: FilePath
tokenName = "tokens"

-- | Produce filename with set and pack amount information
fileName :: PackConfig -> String -> String
fileName cfg name
  | name == packName = printf "%d%s%s.json" (cfg ^. #amount) (cfg ^. #set) name
  | otherwise = printf "%s%s.json" (cfg ^. #set) name
