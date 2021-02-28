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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Encode (encodePacks)
import Generate (encodeFile, genPacks, getLatestCards, readCards)
import Types (fromArgs, getAmount)

main :: IO ()
main = (either print pure =<<) $
  runExceptT $ do
    x <- liftIO $ unwrapRecord ""
    let args :: Args Unwrapped
        args = x
    filePath <- ExceptT getLatestCards
    cards <- ExceptT $ readCards filePath
    selectedCards <- liftIO $ genPacks (getAmount args) (fromArgs args) cards
    _ <- liftIO $ encodeFile "data/packs.json" $ encodePacks selectedCards
    liftIO $ putStrLn "DONE"
