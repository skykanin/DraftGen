{- |
   Module      : File
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : stable
   Portability : portable

 Module for handling reading from and writing to files
-}
module File (execute) where

import CLI
import Control.Concurrent.Async qualified as Async
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Aeson qualified as Json
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Version (showVersion)
import Encode (encodePacks)
import Generate (genLands, genPacks, genTokens)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Paths_DraftGen qualified as Paths
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath ((<.>), (</>))
import Text.Printf (printf)
import Types (CardObj, PackConfig (..), SetDataObj (..), SetInfo (..), fileName, fromArgs)
import Util (appName, landName, packName, tokenName)

execute :: IO ()
execute = (either putStrLn pure <=< runExceptT) $ do
  rawArgs <- liftIO $ unwrapRecord ""
  args <- ExceptT . pure $ validateArgs rawArgs
  let config = fromArgs args
      ln = fileName config landName
      pn = fileName config packName
      tn = fileName config tokenName
  cards <- getFromCache config.set
  dataPath <- liftIO $ getXdgDirectory XdgData appName
  liftIO $ createDirectoryIfMissing True dataPath
  selectedCards <- liftIO $ genPacks config cards
  liftIO $ Json.encodeFile (dataPath </> tn) $ encodePacks $ genTokens config cards
  liftIO $ Json.encodeFile (dataPath </> ln) $ encodePacks $ genLands config cards
  liftIO $ Json.encodeFile (dataPath </> pn) $ encodePacks selectedCards
  liftIO $ printf "Packs generated at: %s\nLands at: %s\nTokens at: %s" (dataPath </> pn) (dataPath </> ln) (dataPath </> tn)

-- | Check that integer arguments aren't negative
validateArgs :: Args Unwrapped -> Either String (Args Unwrapped)
validateArgs as
  | as.amount < 1 = Left "Error: amount is less than one"
  | as.commons < 0 = Left "Error: commons is negative"
  | as.uncommons < 0 = Left "Error: uncommons is negative"
  | as.rares < 0 = Left "Error: rares is negative"
  | otherwise = Right as

getFromCache :: MonadIO m => String -> ExceptT String m (HashSet CardObj)
getFromCache set = do
  cachePrefixPath <- liftIO $ getXdgDirectory XdgCache appName
  let filepath = cachePrefixPath </> set <.> "json"
  setFileExists <- liftIO $ doesFileExist filepath
  if setFileExists
    then ExceptT $ readCards filepath
    else do
      cards <- fetchSet set
      liftIO $ writeSet set cards
      pure cards
 where
  readCards = (fmap . fmap) HS.fromList . liftIO . Json.eitherDecodeFileStrict

writeSet :: String -> HashSet CardObj -> IO ()
writeSet set cards = do
  cachePathPrefix <- liftIO $ getXdgDirectory XdgCache appName
  Json.encodeFile (cachePathPrefix </> set <> ".json") cards

-- Make a GET request to the scryfall API
getScryfall :: Manager -> String -> [(BS.ByteString, Maybe BS.ByteString)] -> IO (Response BSL.ByteString)
getScryfall manager url queryParams = do
  req <- parseRequest url
  let version = BS.pack $ showVersion Paths.version
      req' =
        req
          { requestHeaders =
              [ ("Accept", "application/json")
              , ("User-Agent", "draftgen/" `BS.append` version)
              ]
          }
          & setQueryString queryParams
  httpLbs req' manager

-- Fetch all cards from a given set through scryfall
fetchSet :: MonadIO m => String -> ExceptT String m (HashSet CardObj)
fetchSet set = do
  manager <- liftIO $ newManager tlsManagerSettings
  setInfoRes <- liftIO $ getScryfall manager ("https://api.scryfall.com/sets" </> set) []
  setInfo <- ExceptT . pure $ Json.eitherDecode @SetInfo setInfoRes.responseBody
  let pages :: [Int] =
        enumFromTo 1 $ ceiling $ fromIntegral @_ @Double setInfo.cardCount / 175
      getSetData page =
        getScryfall
          manager
          "https://api.scryfall.com/cards/search"
          [ ("include_extras", Just "true")
          , ("order", Just "set")
          , ("include_multilingual", Just "false")
          , ("include_variations", Just "false")
          , ("unique", Just "prints")
          , ("q", Just $ "e:" `BS.append` BS.pack set)
          , ("page", Just . BS.pack $ show page)
          ]
  results <- liftIO $ Async.mapConcurrently getSetData pages
  cards <- ExceptT . pure $ concatMap (.cardData) <$> traverse (Json.eitherDecode @SetDataObj . (.responseBody)) results
  pure $ HS.fromList cards
