{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

{- |
   Module      : CLI
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Command line arguments parsing module
-}
module CLI (
  Args (..),
  Unwrapped,
  Ratio (..),
  unwrapRecord,
) where

import Data.Char (isNumber)
import Options.Generic (
  Generic,
  Modifiers (shortNameModifier),
  ParseField,
  ParseFields,
  ParseRecord (..),
  Unwrapped,
  Wrapped,
  firstLetter,
  lispCaseModifiers,
  parseRecordWithModifiers,
  unwrapRecord,
  type (:::),
  type (<!>),
  type (<?>),
 )

data Args w = Args
  { set :: w ::: String <!> "m21" <?> "The MTG set to generate cards from (default: m21)"
  , amount :: w ::: Int <!> "6" <?> "Amount of booster packs to generate (default: 6)"
  , commons :: w ::: Int <!> "10" <?> "Amount of commons in pack (default: 10)"
  , uncommons :: w ::: Int <!> "3" <?> "Amount of uncommons in pack (default: 3)"
  , rares :: w ::: Int <!> "1" <?> "Amount of rares in pack (default: 1)"
  , mythicChance :: w ::: Ratio <!> "1/8" <?> "Chance of rare being mythic, value given as ratio (default: 1/8 meaning 1 in 8)"
  , foilChance :: w ::: Ratio <!> "1/45" <?> "Chance of one common being a foil of any rarity, value given as ratio (default: 1/45 meaning 1 in 45)"
  , downloadCards :: w ::: Bool <!> "False" <?> "Update card cache when generating packs"
  , getCard :: w ::: Maybe String <?> "Ignores all other arguments and generates a pack containing the one specific card (no default value)"
  }
  deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers {shortNameModifier = firstLetter}

instance ParseRecord (Args Wrapped) where
  parseRecord = parseRecordWithModifiers modifiers

deriving instance Show (Args Unwrapped)

{- | Orphaned instances for parsing tuple fields
 instance ParseFields (Int, Int)

 instance ParseField (Int, Int)
-}
type Numerator = Int

type Denominator = Int

data Ratio = Ratio Numerator Denominator
  deriving (Eq, Generic)

instance Read Ratio where
  readsPrec _ str = case span isNumber str of
    (x, '/' : rest) -> case span isNumber rest of
      (y, xs) | not $ null y -> [(Ratio (read x) (read y), xs)]
      _ -> []
    _ -> []

instance Show Ratio where
  show (Ratio num den) = show num ++ "/" ++ show den

instance ParseRecord Ratio

instance ParseFields Ratio

instance ParseField Ratio
