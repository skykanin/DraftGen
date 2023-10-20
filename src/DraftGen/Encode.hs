{- |
   Module      : Encode
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Encoding for card representation as TTS object
-}
module Encode (encodeCard, encodePacks) where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Optics
import Types

-- | Encode a single card
encodeCard :: CardObj -> TTSObj
encodeCard card =
  withTTS $
    mkEmptyCard cardTransform
      & #customDeck %~ (Seq.|> mkCardImgObj card)
      & #nickname ?~ card ^. #name

-- | Map position data and set of cards into a GameObj representing a single pack
encodePack :: Foldable f => TransformObj -> f CardObj -> GameObj
encodePack transformObj cardSet =
  go (mkEmptyPack transformObj) 100 $ toList cardSet
 where
  go :: GameObj -> Int -> [CardObj] -> GameObj
  go packObj _ [] = packObj
  go packObj cardId (cardObj : obs) =
    go updatedPackObj (cardId + 100) obs
   where
    updatedPackObj =
      packObj
        & #customDeck %~ (Seq.|> mkCardImgObj cardObj)
        & #deckIDs %~ (Seq.|> cardId)
        & #containedObjects %~ (Seq.|> mkTTSCardObj cardId cardObj)

-- | Encode list of packs into a single TTSObj
encodePacks :: Foldable f => [f CardObj] -> TTSObj
encodePacks = go defaultTTSObj 1 (0, 0)
 where
  go :: Foldable f => TTSObj -> Int -> (Int, Int) -> [f CardObj] -> TTSObj
  go ttsObj _ _ [] = ttsObj
  go ttsObj counter (x, z) (pack : packs) =
    go newTTSObj (checkCounter counter) (checkX x, checkZ z) packs
   where
    newTTSObj =
      ttsObj
        & #objectStates <>~ [encodePack packPosition pack]
    packPosition =
      packTransform
        & #posX +~ x
        & #posZ +~ z
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
    { transform = cardTransform
    , nickname = cardObj ^. #name
    , name = Card
    , cardID = cardId
    }

defaultBackUri :: String
defaultBackUri = "https://upload.wikimedia.org/wikipedia/en/a/aa/Magic_the_gathering-card_back.jpg"

mkCardImgObj :: CardObj -> CardImgObj
mkCardImgObj cardObj =
  CardImgObj
    { backIsHidden = True
    , numWidth = 1
    , numHeight = 1
    , backURL =
        fromMaybe defaultBackUri $
          cardObj ^? #cardFaces % _last % #imageUris %? #png
    , faceURL =
        -- First try to get image from card faces then try base image uri field, otherwise error
        fromMaybe (error "No faceURL found") $
          cardObj ^? #cardFaces % _head % #imageUris %? #png
            <|> cardObj ^? #imageUris %? #png
    }

mkEmptyCard :: TransformObj -> GameObj
mkEmptyCard transformObj =
  GameObj
    { transform = transformObj
    , name = "Card"
    , nickname = Nothing
    , customDeck = Seq.empty
    , cardID = Just 100
    , deckIDs = Seq.empty
    , containedObjects = Seq.empty
    }

mkEmptyPack :: TransformObj -> GameObj
mkEmptyPack transformObj =
  GameObj
    { transform = transformObj
    , name = "DeckCustom"
    , nickname = Nothing
    , customDeck = Seq.empty
    , cardID = Nothing
    , deckIDs = Seq.empty
    , containedObjects = Seq.empty
    }

withTTS :: GameObj -> TTSObj
withTTS gameobj = TTSObj {objectStates = [gameobj]}

defaultTTSObj :: TTSObj
defaultTTSObj =
  TTSObj
    { objectStates =
        [ GameObj
            { transform = packTransform
            , name = "DeckCustom"
            , nickname = Nothing
            , customDeck = Seq.empty
            , cardID = Nothing
            , deckIDs = Seq.empty
            , containedObjects = Seq.empty
            }
        ]
    }
