{-# LANGUAGE FlexibleContexts #-}

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
import Control.Lens
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Types

-- | Encode a single card
encodeCard :: CardObj -> TTSObj
encodeCard card =
  withTTS $
    mkEmptyCard cardTransform
      & customDeck %~ (Seq.|> mkCardImgObj card)
      & nickname ?~ card ^. name

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
            & customDeck %~ (Seq.|> mkCardImgObj cardObj)
            & deckIDs %~ (Seq.|> cardId)
            & containedObjects %~ (Seq.|> mkTTSCardObj cardId cardObj)

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
          cardObj ^? cardFaces . _last . imageUris . _Just . png
    , faceURL =
        -- First try to get image from card faces then try base image uri field, otherwise error
        fromMaybe (error "No faceURL found") $
          cardObj ^? cardFaces . _head . imageUris . _Just . png
            <|> cardObj ^? imageUris . _Just . png
    }

mkEmptyCard :: TransformObj -> GameObj
mkEmptyCard transformObj =
  GameObj
    { gameObjTransform = transformObj
    , gameObjName = "Card"
    , gameObjNickname = Nothing
    , gameObjCustomDeck = Seq.empty
    , gameObjCardID = Just 100
    , gameObjDeckIDs = Seq.empty
    , gameObjContainedObjects = Seq.empty
    }

mkEmptyPack :: TransformObj -> GameObj
mkEmptyPack transformObj =
  GameObj
    { gameObjTransform = transformObj
    , gameObjName = "DeckCustom"
    , gameObjNickname = Nothing
    , gameObjCustomDeck = Seq.empty
    , gameObjCardID = Nothing
    , gameObjDeckIDs = Seq.empty
    , gameObjContainedObjects = Seq.empty
    }

withTTS :: GameObj -> TTSObj
withTTS gameobj = TTSObj {_objectStates = [gameobj]}

defaultTTSObj :: TTSObj
defaultTTSObj =
  TTSObj
    { _objectStates =
        [ GameObj
            { gameObjTransform = packTransform
            , gameObjName = "DeckCustom"
            , gameObjNickname = Nothing
            , gameObjCustomDeck = Seq.empty
            , gameObjCardID = Nothing
            , gameObjDeckIDs = Seq.empty
            , gameObjContainedObjects = Seq.empty
            }
        ]
    }
