{- |
   Module      : Generate
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Generate packs
-}
module Generate (
  dummyTTS,
  encodeFile,
  filterByMTGSet,
  getLatestCards,
  readCards,
  S.size,
) where

import Control.Lens hiding (set)
import Data.Aeson (eitherDecode, eitherDecodeFileStrict, encodeFile)
import qualified Data.ByteString.Lazy as B
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Network.Wreq
import Types

-- | Get the latest card set from scryfall and write them to a json file
getLatestCards :: IO (Either String FilePath)
getLatestCards = do
  r <- get "https://api.scryfall.com/bulk-data/default-cards"
  let eBd :: Either String BulkDataObj
      eBd = eitherDecode $ r ^. responseBody
  case eBd of
    Left err -> pure $ Left err
    Right bulkData -> do
      binData <- get (bulkData ^. downloadUri)
      B.writeFile "data/BulkData.json" (binData ^. responseBody)
      pure $ Right "data/BulkData.json"

-- | Read cards from filepath into memory
readCards :: FilePath -> IO (Either String (HashSet CardObj))
readCards fp = do
  eCards <- eitherDecodeFileStrict fp :: IO (Either String [CardObj])
  pure $ S.fromList <$> eCards

-- | Filter cards by MTG set predicate
filterByMTGSet :: String -> HashSet CardObj -> HashSet CardObj
filterByMTGSet setPred = S.filter (\card -> card ^. set == setPred)

dummyTTS :: TTSObj
dummyTTS =
  TTSObj
    { objectStates =
        [ GameObj
            { gameObjTransform = TransformObj 1 1 1 180 180 0 0 1 0
            , gameObjName = "DeckCustom"
            , gameObjCustomDeck =
                [ CardImgObj
                    { backIsHidden = True
                    , numWidth = 1
                    , numHeight = 1
                    , backURL = "https://upload.wikimedia.org/wikipedia/en/a/aa/Magic_the_gathering-card_back.jpg"
                    , faceURL = "https://c1.scryfall.com/file/scryfall-cards/large/front/4/9/49c07ea0-27ff-46fb-a41f-3e378c977b5d.jpg"
                    }
                , CardImgObj
                    { backIsHidden = True
                    , numWidth = 1
                    , numHeight = 1
                    , backURL = "https://upload.wikimedia.org/wikipedia/en/a/aa/Magic_the_gathering-card_back.jpg"
                    , faceURL = "https://c1.scryfall.com/file/scryfall-cards/large/front/e/3/e3185b01-0d91-4927-98e9-bc9df6c20917.jpg"
                    }
                ]
            , gameObjDeckIDs = [100, 200]
            , gameObjContainedObjects =
                [ TTSCardObj
                    { ttsCardObjTransform = tObj
                    , ttsCardObjNickname = "Duress"
                    , ttsCardObjName = Card
                    , ttsCardObjCardID = 100
                    }
                , TTSCardObj
                    { ttsCardObjTransform = tObj
                    , ttsCardObjNickname = "Massacre Wurm"
                    , ttsCardObjName = Card
                    , ttsCardObjCardID = 200
                    }
                ]
            }
        ]
    }
