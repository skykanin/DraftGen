{- |
   Module      : Main
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Program entry point
-}
module Main where

import CLI (Args, Unwrapped, fromArgs, unwrapRecord)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Generate (encodeAsTTSObj, encodeFile, genPack, getLatestCards, readCards)

main :: IO ()
main = (either print pure =<<) $
  runExceptT $ do
    x <- liftIO $ unwrapRecord ""
    let args :: Args Unwrapped
        args = x
    filePath <- ExceptT getLatestCards
    cards <- ExceptT $ readCards filePath
    selectedCards <- liftIO $ genPack (fromArgs args) cards
    _ <- liftIO $ encodeFile "data/pack.json" $ encodeAsTTSObj selectedCards
    liftIO $ putStrLn "DONE"
