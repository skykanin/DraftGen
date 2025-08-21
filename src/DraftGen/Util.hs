{- |
   Module      : Util
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Module for utility functions
-}
module Util
  ( appName
  , cardCacheName
  , landName
  , packName
  , tokenName
  , splitOn
  )
where

-- | Split a list on a delimiter element. The delimiter is removed.
--   Example: splitOn ',' "a,b,,c" == ["a","b","","c"]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim = go
 where
  go [] = [[]]
  go (x : xs)
    | x == delim = [] : go xs
    | otherwise = case go xs of
        [] -> [[x]]
        (y : ys) -> (x : y) : ys

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
