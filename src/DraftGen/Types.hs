{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Types
License     : GNU GPL, version 3 or above
Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
Stability   : alpha
Portability : portable

Module defining the data types representing cards
-}
module Types
  ( PackConfig (..)
  , Rarity (..)
  , UriObj (..)
  , FrameEffect (..)
  , CardFace (..)
  , CardObj (..)
  , SetInfo (..)
  , SetDataObj (..)
  , CardImgObj (..)
  , ObjType (..)
  , TransformObj (..)
  , TTSCardObj (..)
  , GameObj (..)
  , TTSObj (..)
  , BorderColor (..)
  , fromArgs
  , fileName
  )
where

import CLI (Args (..), Ratio, Unwrapped)
import Data.Aeson
  ( FromJSON (parseJSON)
  , GToJSON
  , KeyValue ((.=))
  , Object
  , Options (constructorTagModifier, fieldLabelModifier)
  , ToJSON (toJSON)
  , Value (Object)
  , camelTo2
  , defaultOptions
  , genericParseJSON
  , genericToJSON
  , object
  , withObject
  , (.!=)
  , (.:)
  , (.:?)
  )
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap qualified as M
import Data.Aeson.Types (GFromJSON, Zero)
import Data.Char (toLower)
import Data.Hashable (Hashable)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Generics (Generic (..))
import Text.Printf (printf)
import Util (packName, splitOn)

newtype ConfigJSON (opts :: JsonOption) a = ConfigJSON a

type data JsonOption = SnakeCase | LowerCase | StripPrefix

class HasJsonOptions tag where
  jsonOptions :: Options

instance HasJsonOptions SnakeCase where
  jsonOptions =
    defaultOptions
      { fieldLabelModifier = camelTo2 '_'
      , constructorTagModifier = toLowerCase
      }

instance HasJsonOptions LowerCase where
  jsonOptions =
    defaultOptions
      { constructorTagModifier = toLowerCase
      , fieldLabelModifier = toLowerCase
      }

-- | Strips a camelcased prefix of constructor tag and lowercases it
--
-- >>> constructorTagModifier "ColorRed"
-- "red"
instance HasJsonOptions StripPrefix where
  jsonOptions =
    defaultOptions
      { constructorTagModifier =
          toLowerCase . (!! 1) . splitOn '_' . camelTo2 '_'
      }

instance
  (Generic a, GToJSON Zero (Rep a), HasJsonOptions tag)
  => ToJSON (ConfigJSON tag a)
  where
  toJSON = genericToJSON (jsonOptions @tag) . (\(ConfigJSON x) -> x)

instance
  (Generic a, GFromJSON Zero (Rep a), HasJsonOptions tag)
  => FromJSON (ConfigJSON tag a)
  where
  parseJSON = fmap ConfigJSON . genericParseJSON (jsonOptions @tag)

data PackConfig = PackConfig
  { amount :: Int
  , set :: String
  , commons :: Int
  , uncommons :: Int
  , rareOrMythics :: Int
  , mythicChance :: Ratio
  , foilChance :: Ratio
  }
  deriving stock (Generic, Show)

-- | Produce filename with set and pack amount information
fileName :: PackConfig -> String -> String
fileName cfg name
  | name == packName = printf "%d%s%s.json" cfg.amount cfg.set name
  | otherwise = printf "%s%s.json" cfg.set name

fromArgs :: Args Unwrapped -> PackConfig
fromArgs (Args s a c uc r mc fc) = PackConfig a s c uc r mc fc

-- Lowercase string
toLowerCase :: String -> String
toLowerCase = map toLower

data Rarity = Common | Uncommon | Rare | Mythic | Special | Bonus
  deriving stock (Enum, Eq, Generic, Show)
  deriving anyclass (Hashable)
  deriving (FromJSON, ToJSON) via (ConfigJSON LowerCase Rarity)

data UriObj = UriObj
  { small :: String
  , normal :: String
  , large :: String
  , png :: String
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)
  deriving anyclass (FromJSON, ToJSON)

data FrameEffect
  = Legendary
  | Miracle
  | Nyxtouched
  | Draft
  | Devoid
  | Tombstone
  | Colorshifted
  | Inverted
  | SunMoonDfc
  | CompassLandDfc
  | OriginPwDfc
  | MoonEldraziDfc
  | WaxingAndWaningMoonDfc
  | Showcase
  | ExtendedArt
  | Enchantment
  | Spree
  | Companion
  | Etched
  | Snow
  | Lesson
  | Shatteredglass
  | ConvertDfc
  | FanDfc
  | UpsideDownDfc
  | Borderless
  | Textless
  | Fullart
  | Vehicle
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)
  deriving (FromJSON, ToJSON) via (ConfigJSON LowerCase FrameEffect)

data CardFace = CardFace
  { name :: String
  , imageUris :: Maybe UriObj
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)
  deriving (FromJSON, ToJSON) via (ConfigJSON SnakeCase CardFace)

data CardObj = CardObj
  { id :: String
  , name :: String
  , lang :: String
  , layout :: String -- Make sum type for this
  , highresImage :: Bool
  , imageUris :: Maybe UriObj
  , cardFaces :: List CardFace
  , typeLine :: String
  , frameEffects :: List FrameEffect
  , borderColor :: BorderColor
  , set :: String
  , cmc :: Double
  , foil :: Bool
  , promo :: Bool
  , reprint :: Bool
  , variation :: Bool
  , fullArt :: Bool
  , rarity :: Rarity
  }
  deriving stock (Generic, Show)
  deriving anyclass (Hashable)
  deriving (ToJSON) via (ConfigJSON SnakeCase CardObj)

-- | Check card equality only by name
instance Eq CardObj where
  cardObjA == cardObjB =
    cardObjA.name == cardObjB.name

instance FromJSON CardObj where
  parseJSON = withObject "CardObj" $ \v ->
    CardObj
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "lang"
      <*> v .: "layout"
      <*> v .: "highres_image"
      <*> v .:? "image_uris"
      <*> v .:? "card_faces" .!= []
      <*> v .:? "type_line" .!= ""
      <*> v .:? "frame_effects" .!= []
      <*> v .: "border_color"
      <*> v .: "set"
      <*> v .:? "cmc" .!= 0
      <*> v .: "foil"
      <*> v .: "promo"
      <*> v .: "reprint"
      <*> v .: "variation"
      <*> v .: "full_art"
      <*> v .: "rarity"

data BorderColor
  = ColorBlack
  | ColorWhite
  | ColorSilver
  | ColorGold
  | ColorBorderless
  | ColorYellow
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)
  deriving (FromJSON, ToJSON) via (ConfigJSON StripPrefix BorderColor)

data SetInfo = SetInfo
  { id :: String
  , code :: String
  , searchUri :: String
  , releasedAt :: String
  , setType :: String
  , cardCount :: Int
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via (ConfigJSON SnakeCase SetInfo)

data SetDataObj = SetDataObj
  { object :: String
  , totalCards :: Int
  , hasMore :: Bool
  , cardData :: [CardObj]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON) via (ConfigJSON SnakeCase SetDataObj)

toObject :: Seq CardImgObj -> Value
toObject = Object . go 1 M.empty
 where
  go :: Int -> Object -> Seq CardImgObj -> Object
  go _ m Seq.Empty = m
  go n m (x Seq.:<| xs) =
    go (n + 1) (M.insert (fromString $ show n) (toJSON x) m) xs

data CardImgObj = CardImgObj
  { backIsHidden :: Bool
  , numWidth :: Int
  , numHeight :: Int
  , backURL :: String
  , faceURL :: String
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data ObjType = Card
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data TransformObj = TransformObj
  { scaleZ :: Int
  , scaleY :: Int
  , scaleX :: Int
  , rotZ :: Int
  , rotY :: Int
  , rotX :: Int
  , posZ :: Int
  , posY :: Int
  , posX :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data TTSCardObj = TTSCardObj
  { transform :: TransformObj
  , nickname :: String
  , name :: ObjType
  , cardID :: Int
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via (ConfigJSON LowerCase TTSCardObj)

data GameObj = GameObj
  { transform :: TransformObj
  , name :: String
  , nickname :: Maybe String
  , customDeck :: Seq CardImgObj
  , cardID :: Maybe Int
  , deckIDs :: Seq Int
  , containedObjects :: Seq TTSCardObj
  }
  deriving stock (Generic, Show)

instance ToJSON GameObj where
  toJSON (GameObj t n nn cd cId dIds co) =
    object $
      [ "transform" .= t
      , "name" .= n
      , "customDeck" .= toObject cd
      , "deckIDs" .= dIds
      , "containedObjects" .= co
      ]
        ++ cardIdField
        ++ nicknameField
   where
    cardIdField = maybe [] (pure . ("CardID" .=)) cId
    nicknameField = maybe [] (pure . ("Nickname" .=)) nn

newtype TTSObj = TTSObj
  {objectStates :: List GameObj}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)
