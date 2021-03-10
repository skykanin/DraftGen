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
import Encode (encodePacks)
import Generate (genLands, genPacks, readCards)
import Network.Wreq (get, responseBody)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)
import Types (BulkDataObj, downloadUri, fromArgs)
import Util (fileName)

appName :: FilePath
appName = "DraftGen"

landCacheName :: FilePath
landCacheName = "LandData.json"

cardCacheName :: FilePath
cardCacheName = "CardData.json"

landName :: FilePath
landName = "lands"

packName :: FilePath
packName = "packs"

-- | Check that integer arguments aren't negative
validateArgs :: Args Unwrapped -> Either String (Args Unwrapped)
validateArgs as
  | amount as < 1 = Left "Error: amount is less that one"
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
    (landCache, cardCache) <-
      ExceptT $ getFromCache (updateCards args) (cachePath </> landCacheName) (cachePath </> cardCacheName)
    cards <- ExceptT $ readCards cardCache
    landData <- ExceptT $ readCards landCache
    let config = fromArgs args
        ln = fileName config landName
        pn = fileName config packName
    dataPath <- liftIO $ getXdgDirectory XdgData appName
    _ <- liftIO $ createDirectoryIfMissing True dataPath
    selectedCards <- liftIO $ genPacks config cards
    _ <- liftIO $ encodeFile (dataPath </> ln) $ encodePacks $ genLands config landData
    _ <- liftIO $ encodeFile (dataPath </> pn) $ encodePacks selectedCards
    liftIO $ printf "Packs generated at: %s\nLands at: %s" (dataPath </> pn) (dataPath </> ln)

-- | If land and card cache already exists return them unless a flush is forced otherwise fetch them from scryfall
getFromCache :: Bool -> FilePath -> FilePath -> IO (Either String (FilePath, FilePath))
getFromCache force landPath cardPath = paths >>= choice'
  where
    choice' = uncurry (choice force)
    paths = (,) <$> doesFileExist landPath <*> doesFileExist cardPath
    choice toForce landExists cardExists
      | toForce = updateCache
      | landExists && cardExists = pure $ Right (landPath, cardPath)
      | otherwise = updateCache
      where
        msg = putStrLn "Updating cache..."
        updateCache = msg *> getLatestCards landPath cardPath

-- | Get the latest card set from scryfall and write them to a json file
getLatestCards :: FilePath -> FilePath -> IO (Either String (FilePath, FilePath))
getLatestCards landPath bulkPath = do
  l <- get "https://api.scryfall.com/bulk-data/default-cards"
  r <- get "https://api.scryfall.com/bulk-data/oracle-cards"
  let eBd :: Either String BulkDataObj
      eBd = eitherDecode $ r ^. responseBody
      eLands :: Either String BulkDataObj
      eLands = eitherDecode $ l ^. responseBody
  case (eLands, eBd) of
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err
    (Right landData, Right bulkData) -> do
      binData <- get (bulkData ^. downloadUri)
      landBinData <- get (landData ^. downloadUri)
      B.writeFile landPath (landBinData ^. responseBody)
      B.writeFile bulkPath (binData ^. responseBody)
      pure $ Right (landPath, bulkPath)
