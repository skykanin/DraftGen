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
  Args,
  Unwrapped,
  fromArgs,
  getAmount,
  unwrapRecord,
) where

import Options.Generic
import Types (PackConfig (..), Ratio)

data Args w = Args
  { set :: w ::: String <!> "m21" <?> "The MTG set to generate cards from (default: m21)"
  , amount :: w ::: Int <!> "6" <?> "Amount of booster packs to generate (default: 6)"
  , commons :: w ::: Int <!> "10" <?> "Amount of commons in pack (default: 10)"
  , uncommons :: w ::: Int <!> "3" <?> "Amount of uncommons in pack (default: 3)"
  , rares :: w ::: Int <!> "1" <?> "Amount of rares in pack (default: 1)"
  , mythicChance :: w ::: Ratio <!> "(1, 8)" <?> "Chance of rare being mythic, value given as ratio (default: (1, 8) meaning 1 in 8)"
  , foilChance :: w ::: Ratio <!> "(1, 45)" <?> "Chance of one common being a foil of any rarity, value given as ratio (default: (1, 45) meaning 1 in 45)"
  , directory :: w ::: String <!> "BoosterPacks" <?> "Path to output directory (default: BoosterPacks)"
  , updateCards :: w ::: Bool <!> "False" <?> "Update card inventory by downloading the cards from scryfall (default: False)"
  }
  deriving (Generic)

modifiers :: Modifiers
modifiers =
  defaultModifiers
    { shortNameModifier = firstLetter
    }

instance ParseRecord (Args Wrapped) where
  parseRecord = parseRecordWithModifiers modifiers

deriving instance Show (Args Unwrapped)

-- | Orphaned instances for parsing tuple fields
instance ParseFields (Int, Int)

instance ParseField (Int, Int)

fromArgs :: Args Unwrapped -> PackConfig
fromArgs (Args s _ c uc r mc fc _ _) = PackConfig s c uc r mc fc

getAmount :: Args Unwrapped -> Int
getAmount (Args _ amt _ _ _ _ _ _ _) = amt
