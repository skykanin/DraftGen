{- |
   Module      : Main
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Program entry point
-}
module Main where

import CLI (Args, Unwrapped, unwrapRecord)
import Generate (dummyTTS, encodeFile, filterByMTGSet, getLatestCards, readCards, size)

main :: IO ()
main = do
  encodeFile "data/testing.json" dummyTTS
  putStrLn "DONE"

-- main :: IO ()
-- main = (either print pure =<<) $ runExceptT $ do
--   x <- ExceptT unwrapRecord ""
--   let args :: Args Unwrapped
--       args = x
--   filePath <- ExceptT getLatestCards
--   cards <- ExceptT $ readCards filePath
--   lift $ print $ size $ filterByMTGSet "m21" cards
