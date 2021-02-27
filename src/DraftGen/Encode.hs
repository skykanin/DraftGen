{-# LANGUAGE FlexibleContexts #-}

{- |
   Module      : Encode
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Encoding for card representation as TTS object
-}
module Encode (encodePacks) where

import Control.Lens
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Maybe (fromMaybe)
import Types

-- | Map position data and set of cards into a GameObj representing a single pack
encodePack :: TransformObj -> HashSet CardObj -> GameObj
encodePack transformObj cardSet =
  go (mkEmptyPack transformObj) 100 $ S.toList cardSet
  where
    go :: GameObj -> Int -> [CardObj] -> GameObj
    go packObj _ [] = packObj
    go packObj cardId (cardObj : obs) =
      go updatedPackObj (cardId + 100) obs
      where
        updatedPackObj =
          packObj
            & customDeck <>~ [mkCardImgObj cardObj]
            & deckIDs <>~ [cardId]
            & containedObjects <>~ [mkTTSCardObj cardId cardObj]

-- | Encode list of packs into a single TTSObj
encodePacks :: [HashSet CardObj] -> TTSObj
encodePacks = go defaultTTSObj 1 (0, 0)
  where
    go :: TTSObj -> Int -> (Int, Int) -> [HashSet CardObj] -> TTSObj
    go ttsObj _ _ [] = ttsObj
    go ttsObj counter (x, z) (pack : packs) =
      go newTTSObj (checkCounter counter) (checkX x, checkZ z) packs
      where
        newTTSObj =
          ttsObj
            & objectStates <>~ [encodePack packPosition pack]
        packPosition =
          packTransform
            & posX +~ x
            & posZ +~ z
        checkX xVal
          | counter `mod` cutOff == 0 && counter > 0 = 0
          | otherwise = xInc + xVal
        checkZ zVal
          | counter `mod` cutOff == 0 && counter > 0 = zInc + zVal
          | otherwise = zVal
        checkCounter n
          | n `mod` cutOff == 0 = 1
          | otherwise = n + 1

cutOff :: Int
cutOff = 6

xInc :: Int
xInc = 3

zInc :: Int
zInc = -4

cardTransform :: TransformObj
cardTransform = TransformObj 1 1 1 180 180 0 0 0 0

packTransform :: TransformObj
packTransform = TransformObj 1 1 1 180 180 0 0 1 0

mkTTSCardObj :: Int -> CardObj -> TTSCardObj
mkTTSCardObj cardId cardObj =
  TTSCardObj
    { ttsCardObjTransform = cardTransform
    , ttsCardObjNickname = cardObj ^. name
    , ttsCardObjName = Card
    , ttsCardObjCardID = cardId
    }

mkCardImgObj :: CardObj -> CardImgObj
mkCardImgObj cardObj =
  CardImgObj
    { backIsHidden = True
    , numWidth = 1
    , numHeight = 1
    , backURL = "https://upload.wikimedia.org/wikipedia/en/a/aa/Magic_the_gathering-card_back.jpg"
    , faceURL =
        fromMaybe (error "No faceURL") $
          cardObj ^? imageUris . _Just . large
    }

mkEmptyPack :: TransformObj -> GameObj
mkEmptyPack transformObj =
  GameObj
    { gameObjTransform = transformObj
    , gameObjName = "DeckCustom"
    , gameObjCustomDeck = []
    , gameObjDeckIDs = []
    , gameObjContainedObjects = []
    }

defaultTTSObj :: TTSObj
defaultTTSObj =
  TTSObj
    { _objectStates =
        [ GameObj
            { gameObjTransform = packTransform
            , gameObjName = "DeckCustom"
            , gameObjCustomDeck = []
            , gameObjDeckIDs = []
            , gameObjContainedObjects = []
            }
        ]
    }
