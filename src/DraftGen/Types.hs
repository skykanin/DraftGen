{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
   Module      : Types
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Module defining the data types representing cards
-}
module Types where

import CLI (Args (..), Ratio, Unwrapped)
import Control.Lens hiding (Empty, Unwrapped, (.=))
import Data.Aeson
import Data.Char (toLower)
import Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.Sequence (Seq (..))
import Data.Text (pack, unpack)
import GHC.Generics

data PackConfig = PackConfig
  { packConfigAmount :: Int
  , packConfigSet :: String
  , packConfigCommons :: Int
  , packConfigUncommons :: Int
  , packConfigRareOrMythics :: Int
  , packConfigMythicChance :: Ratio
  , packConfigFoilChance :: Ratio
  }
  deriving (Generic, Show)

makeFields ''PackConfig

fromArgs :: Args Unwrapped -> PackConfig
fromArgs (Args s a c uc r mc fc _) = PackConfig a s c uc r mc fc

data Rarity = Common | Uncommon | Rare | Mythic | Special | Bonus
  deriving (Enum, Eq, Generic, Show)

instance Hashable Rarity

instance FromJSON Rarity where
  parseJSON (String s) = pure $ case unpack s of
    "common" -> Common
    "uncommon" -> Uncommon
    "rare" -> Rare
    "mythic" -> Mythic
    "special" -> Special
    "bonus" -> Bonus
    _ -> error "Unexpected rarity string"
  parseJSON _ = error "Rarity is of unexpected type"

data UriObj = UriObj
  { uriObjSmall :: String
  , uriObjNormal :: String
  , uriObjLarge :: String
  , uriObjPng :: String
  }
  deriving (Eq, Generic, Show)

makeFields ''UriObj

instance Hashable UriObj

instance FromJSON UriObj where
  parseJSON = withObject "UriObj" $ \v ->
    UriObj
      <$> v .: "small"
      <*> v .: "normal"
      <*> v .: "large"
      <*> v .: "png"

data CardObj = CardObj
  { cardObjId :: String
  , cardObjName :: String
  , cardObjLang :: String
  , cardObjLayout :: String
  , cardObjHighresImage :: Bool
  , cardObjImageUris :: Maybe UriObj
  , cardObjTypeLine :: String
  , cardObjSet :: String
  , cardObjCmc :: Double
  , cardObjFoil :: Bool
  , cardObjPromo :: Bool
  , cardObjReprint :: Bool
  , cardObjVariation :: Bool
  , cardObjRarity :: Rarity
  }
  deriving (Generic, Show)

makeFields ''CardObj

instance Hashable CardObj

-- | Check card equality only by name
instance Eq CardObj where
  cardObjA == cardObjB =
    cardObjA ^. name == cardObjB ^. name

instance FromJSON CardObj where
  parseJSON = withObject "CardObj" $ \v ->
    CardObj
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "lang"
      <*> v .: "layout"
      <*> v .: "highres_image"
      <*> v .:? "image_uris"
      <*> v .: "type_line"
      <*> v .: "set"
      <*> v .: "cmc"
      <*> v .: "foil"
      <*> v .: "promo"
      <*> v .: "reprint"
      <*> v .: "variation"
      <*> v .: "rarity"

data BulkDataObj = BulkDataObj
  { bulkDataObjId :: String
  , bulkDataObjBulkType :: String
  , bulkDataObjName :: String
  , bulkDataObjDownloadUri :: String
  }
  deriving (Generic, Show)

makeFields ''BulkDataObj

instance FromJSON BulkDataObj where
  parseJSON = withObject "BulkDataObj" $ \v ->
    BulkDataObj
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "name"
      <*> v .: "download_uri"

toObject :: Seq CardImgObj -> Value
toObject = Object . go 1 M.empty
  where
    go :: Int -> Object -> Seq CardImgObj -> Object
    go _ m Empty = m
    go n m (x :<| xs) =
      go (n + 1) (M.insert (pack $ show n) (toJSON x) m) xs

data CardImgObj = CardImgObj
  { backIsHidden :: Bool
  , numWidth :: Int
  , numHeight :: Int
  , backURL :: String
  , faceURL :: String
  }
  deriving (Generic, Show)

instance ToJSON CardImgObj

data ObjType = Card
  deriving (Generic, Show)

instance ToJSON ObjType where
  toJSON Card = String "Card"

data TransformObj = TransformObj
  { _scaleZ :: Int
  , _scaleY :: Int
  , _scaleX :: Int
  , _rotZ :: Int
  , _rotY :: Int
  , _rotX :: Int
  , _posZ :: Int
  , _posY :: Int
  , _posX :: Int
  }
  deriving (Generic, Show)

makeFieldsNoPrefix ''TransformObj

instance ToJSON TransformObj where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = tail}

data TTSCardObj = TTSCardObj
  { ttsCardObjTransform :: TransformObj
  , ttsCardObjNickname :: String
  , ttsCardObjName :: ObjType
  , ttsCardObjCardID :: Int
  }
  deriving (Generic, Show)

instance ToJSON TTSCardObj where
  toJSON =
    genericToJSON $
      defaultOptions {fieldLabelModifier = lower . drop 10}
    where
      lower [] = []
      lower (x : xs) = toLower x : xs

data GameObj = GameObj
  { gameObjTransform :: TransformObj
  , gameObjName :: String
  , gameObjCustomDeck :: Seq CardImgObj
  , gameObjDeckIDs :: Seq Int
  , gameObjContainedObjects :: Seq TTSCardObj
  }
  deriving (Generic, Show)

makeFields ''GameObj

instance ToJSON GameObj where
  toJSON (GameObj t n cd dIds co) =
    object
      [ "transform" .= t
      , "name" .= n
      , "customDeck" .= toObject cd
      , "deckIDs" .= dIds
      , "containedObjects" .= co
      ]

newtype TTSObj = TTSObj
  {_objectStates :: [GameObj]}
  deriving (Generic, Show)

makeFieldsNoPrefix ''TTSObj

instance ToJSON TTSObj where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = tail}
