{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
    getRecord,
) where

import Options.Generic

data Args = Args
    { set :: String <?> "The MTG set to generate cards from"
    , amount :: Int <?> "Amount of booster packs to generate"
    }
    deriving (Generic, Show)

instance ParseRecord Args
