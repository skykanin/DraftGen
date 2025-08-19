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
  , snakeCase
  , tokenName
  )
where

import Data.Char

snakeCase :: String -> String
snakeCase = symbCase '_'

-- | Generic casing for symbol separated names
symbCase :: Char -> (String -> String)
symbCase sym = u . applyFirst toLower
 where
  u [] = []
  u (x : xs)
    | isUpper x = sym : toLower x : u xs
    | otherwise = x : u xs

applyFirst :: (Char -> Char) -> String -> String
applyFirst _ [] = []
applyFirst f [x] = [f x]
applyFirst f (x : xs) = f x : xs

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
