{- |
   Module      : File
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Module for handling reading from and writing to files
-}
module File (execute) where

import CLI (Args (..), Unwrapped, unwrapRecord)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson (eitherDecode, encodeFile)
import qualified Data.ByteString.Lazy as B
import Encode (encodeCard, encodePacks)
import Generate (findCard, genLands, genPacks, genTokens, readCards)
import Network.Wreq (get, responseBody)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)
import Types (BulkDataObj, CardObj, downloadUri, fromArgs)
import Util (appName, cardCacheName, fileName, landName, packName, tokenName)

-- | Check that integer arguments aren't negative
validateArgs :: Args Unwrapped -> Either String (Args Unwrapped)
validateArgs as
  | amount as < 1 = Left "Error: amount is less than one"
  | commons as < 0 = Left "Error: commons is negative"
  | uncommons as < 0 = Left "Error: uncommons is negative"
  | rares as < 0 = Left "Error: rares is negative"
  | otherwise = Right as

execute :: IO ()
execute = (either print pure =<<) $
  runExceptT $ do
    x <- liftIO $ unwrapRecord ""
    args <- ExceptT $ pure $ validateArgs x
    cachePath <- liftIO $ getXdgDirectory XdgCache appName
    _ <- liftIO $ createDirectoryIfMissing True cachePath
    cardCache <-
      ExceptT $ getFromCache (downloadCards args) (cachePath </> cardCacheName)
    cards <- ExceptT $ readCards cardCache
    -- If argument is passed to a card search otherwise generate packs
    case getCard args of
      Just query -> liftIO $ searchCard query cards
      Nothing -> do
        let config = fromArgs args
            ln = fileName config landName
            pn = fileName config packName
            tn = fileName config tokenName
        dataPath <- liftIO $ getXdgDirectory XdgData appName
        _ <- liftIO $ createDirectoryIfMissing True dataPath
        selectedCards <- liftIO $ genPacks config cards
        _ <- liftIO $ encodeFile (dataPath </> tn) $ encodePacks $ genTokens config cards
        _ <- liftIO $ encodeFile (dataPath </> ln) $ encodePacks $ genLands config cards
        _ <- liftIO $ encodeFile (dataPath </> pn) $ encodePacks selectedCards
        liftIO $ printf "Packs generated at: %s\nLands at: %s\nTokens at: %s" (dataPath </> pn) (dataPath </> ln) (dataPath </> tn)

-- | Find card and write to file if it exists
searchCard :: String -> [CardObj] -> IO ()
searchCard query cards = do
  case findCard query cards of
    Nothing -> putStrLn "Card not found"
    Just card -> do
      dataPath <- liftIO $ getXdgDirectory XdgData appName
      let cardObj = encodeCard card
          filePath = dataPath </> query ++ ".json"
      encodeFile filePath cardObj
      printf "Card generated at: %s" filePath

-- | If card cache already exists return them unless a flush is forced otherwise fetch them from scryfall
getFromCache :: Bool -> FilePath -> IO (Either String FilePath)
getFromCache force cardPath = doesFileExist cardPath >>= choice force
  where
    choice toForce cardsExists
      | toForce = updateCache
      | cardsExists = pure $ Right cardPath
      | otherwise = updateCache
      where
        msg = putStrLn "Updating cache..."
        updateCache = msg *> getLatestCards cardPath

-- | Get the latest card set from scryfall and write it to a json file
getLatestCards :: FilePath -> IO (Either String FilePath)
getLatestCards cardPath = do
  c <- get "https://api.scryfall.com/bulk-data/default-cards"
  let eCards :: Either String BulkDataObj
      eCards = eitherDecode $ c ^. responseBody
  case eCards of
    Left err -> pure $ Left err
    Right cardData -> do
      cardBinData <- get (cardData ^. downloadUri)
      B.writeFile cardPath (cardBinData ^. responseBody)
      pure $ Right cardPath
