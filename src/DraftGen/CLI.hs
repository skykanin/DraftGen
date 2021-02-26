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
  unwrapRecord,
) where

import Options.Generic

data Args w = Args
  { set :: w ::: String <!> "m21" <?> "The MTG set to generate cards from (default: m21)"
  , amount :: w ::: Int <!> "6" <?> "Amount of booster packs to generate (default: 6)"
  , directory :: w ::: String <!> "Booster Packs" <?> "Path to output directory (default: Booster Packs)"
  }
  deriving (Generic)

instance ParseRecord (Args Wrapped)

deriving instance Show (Args Unwrapped)
