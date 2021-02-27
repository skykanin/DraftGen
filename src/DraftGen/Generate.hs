{-# LANGUAGE FlexibleContexts #-}

{- |
   Module      : Generate
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Generate packs
-}
module Generate (
  encodeFile,
  filterBySet,
  genPacks,
  getLatestCards,
  readCards,
  S.size,
) where

import Control.Lens hiding (set)
import Control.Monad (replicateM)
import Data.Aeson (eitherDecode, eitherDecodeFileStrict, encodeFile)
import qualified Data.ByteString.Lazy as B
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust)
import Network.Wreq
import System.Random
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
filterBySet :: String -> HashSet CardObj -> HashSet CardObj
filterBySet setPred = S.filter (\card -> card ^. set == setPred)

-- | Undesired card layouts
notDesired :: [String]
notDesired = ["planar", "scheme", "vanguard", "token", "double_faced_token", "emblem"]

filterDesired :: HashSet CardObj -> HashSet CardObj
filterDesired = S.filter (\card -> desired card && hasFaceURL card)
  where
    desired card = card ^. layout `notElem` notDesired
    hasFaceURL card = isJust $ card ^. imageUris

-- | Filter cards by MTG rarity
filterByRarity :: Rarity -> HashSet CardObj -> HashSet CardObj
filterByRarity rarPred = S.filter (\card -> card ^. rarity == rarPred)

data Include = In | Out

-- | Filter on MTG basic lands
filterBasicLands :: Include -> HashSet CardObj -> HashSet CardObj
filterBasicLands incl =
  S.filter (\card -> p incl $ checkBasic card && checkLand card)
  where
    checkBasic c = "Basic" `isPrefixOf` (c ^. typeLine)
    checkLand c = "Land" `isInfixOf` (c ^. typeLine)
    p In = Prelude.id
    p Out = not

-- | Pick a rarity to choose from based on the mythic drop chance in the pack configuration
pickRarity :: Ratio -> IO Rarity
pickRarity (numerator, denominator) = do
  chance <- getStdRandom (randomR (1, denominator))
  pure $ if chance > numerator then Rare else Mythic

-- | List of all the MTG rarities
rarities :: [Rarity]
rarities = [Common .. Mythic]

-- | Generate cards of common rarity with a chance of one being a foil of any rarity
commonWithMaybeFoil :: Ratio -> Int -> HashSet CardObj -> HashSet CardObj -> IO (HashSet CardObj)
commonWithMaybeFoil (numerator, denominator) amount commonSet foilSet = do
  chance <- getStdRandom (randomR (1, denominator))
  if chance > numerator
    then gen amount commonSet
    else do
      randRarity <- getStdRandom (randomR (0, 3))
      let selectedRarity = rarities !! randRarity
          filteredSet = filterByRarity selectedRarity foilSet
      foilCard <- gen 1 filteredSet
      commonCards <- gen (amount - 1) commonSet
      pure $ commonCards `S.union` foilCard

type Amount = Int

-- | Plural of genPack
genPacks :: Amount -> PackConfig -> HashSet CardObj -> IO [HashSet CardObj]
genPacks amount config cards = replicateM amount (genPack config cards)

-- | Generate a random pack based on the pack configuration
genPack :: PackConfig -> HashSet CardObj -> IO (HashSet CardObj)
genPack config cards = do
  let setCards = english . filterBySet (config ^. set) $ cards
      base = filterDesired . filterBasicLands Out $ setCards
      english = S.filter (\c -> c ^. lang == "en")
      fbr r = filterByRarity r base
      foils = S.filter (^. foil) base
      lands = filterBasicLands In setCards
  commonWithMaybeFoilCards <-
    commonWithMaybeFoil (config ^. foilChance) (config ^. commons) (fbr Common) foils
  uncommonCards <- gen (config ^. uncommons) (fbr Uncommon)
  pick <- pickRarity (config ^. mythicChance)
  rareOrMythicCards <- gen (config ^. rareOrMythics) (fbr pick)
  basicLand <- gen 1 lands
  pure $ S.unions [commonWithMaybeFoilCards, uncommonCards, rareOrMythicCards, basicLand]

-- | Generate set of n cards from set
gen :: Int -> HashSet CardObj -> IO (HashSet CardObj)
gen amount cards = go amount cards S.empty
  where
    go n pool acc
      | n <= 0 || S.null pool = pure acc
      | otherwise = do
        rand <- getStdRandom (randomR (0, S.size pool - 1))
        let card = S.toList pool !! rand
        go (n - 1) (S.delete card pool) (S.insert card acc)
