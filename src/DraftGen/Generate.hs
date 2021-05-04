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
  genLands,
  genPacks,
  genTokens,
  readCards,
  S.size,
) where

import CLI (Ratio (..))
import Control.Lens hiding (set)
import Control.Monad (replicateM)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (intersect, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (isJust)
import System.Random
import Types

-- | Read cards from filepath into memory
readCards :: FilePath -> IO (Either String [CardObj])
readCards fp = do
  eCards <- eitherDecodeFileStrict fp :: IO (Either String [CardObj])
  pure $ eCards

-- | Filter cards by MTG set predicate
filterBySet :: String -> HashSet CardObj -> HashSet CardObj
filterBySet mtgSet = S.filter (\card -> card ^. set == mtgSet)

-- | Undesired card layouts
unwantedLayout :: [String]
unwantedLayout = ["planar", "scheme", "vanguard", "token", "double_faced_token", "emblem"]

-- | No weird frame effects pls
unwantedFrameEffects :: [FrameEffect]
unwantedFrameEffects = [Draft, ExtendedArt, Showcase]

filterDesired :: [CardObj] -> HashSet CardObj
filterDesired =
  S.fromList
    . filter
      ( \card ->
          desiredLayout card
            && desiredFrameEff card
            && hasFaceURL card
            && nonVar card
      )
  where
    nonVar card = not $ card ^. variation
    desiredLayout card = card ^. layout `notElem` unwantedLayout
    desiredFrameEff card = (card ^. frameEffects) `intersect` unwantedFrameEffects == []
    hasFaceURL card = isJust $ card ^. imageUris

-- | Filter cards by MTG rarity
filterByRarity :: Rarity -> HashSet CardObj -> HashSet CardObj
filterByRarity rarPred = S.filter (\card -> card ^. rarity == rarPred)

data Include = In | Out

-- | Filter on MTG type line 'lesson'
filterLesson :: Include -> HashSet CardObj -> HashSet CardObj
filterLesson incl =
  S.filter (\card -> p incl $ isLesson card)
  where
    isLesson c = "Lesson" `isSuffixOf` (c ^. typeLine)
    p In = Prelude.id
    p Out = not

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
pickRareOrMythic :: Ratio -> IO Rarity
pickRareOrMythic (Ratio numerator denominator) = do
  chance <- getStdRandom (randomR (1, denominator))
  pure $ if chance > numerator then Rare else Mythic

pickRarity :: [Rarity] -> IO Rarity
pickRarity rarityList = do
  idx <- getStdRandom (randomR (0, length rarityList - 1))
  pure $ rarityList !! idx

-- | List of all the MTG rarities
rarities :: [Rarity]
rarities = [Common .. Mythic]

-- | Generate cards of common rarity with a chance of one being a foil of any rarity
commonWithMaybeFoil :: Ratio -> Int -> HashSet CardObj -> HashSet CardObj -> IO (HashSet CardObj)
commonWithMaybeFoil (Ratio numerator denominator) n commonSet foilSet = do
  chance <- getStdRandom (randomR (1, denominator))
  if chance > numerator
    then gen n commonSet
    else do
      randRarity <- getStdRandom (randomR (0, 3))
      let selectedRarity = rarities !! randRarity
          filteredSet = filterByRarity selectedRarity foilSet
      foilCard <- gen 1 filteredSet
      commonCards <- gen (n - 1) commonSet
      pure $ commonCards `S.union` foilCard

-- | Plural of genPack
genPacks :: PackConfig -> [CardObj] -> IO [HashSet CardObj]
genPacks config cards = replicateM (config ^. amount) (genPack config cards)

-- | Return basic lands belonging to the data set
genLands :: PackConfig -> [CardObj] -> [HashSet CardObj]
genLands config = pure . filterBasicLands In . filterBySet (config ^. set) . S.fromList

-- | Generate the token cards for a given set
genTokens :: PackConfig -> [CardObj] -> [HashSet CardObj]
genTokens config = pure . filterBySet ('t' : config ^. set) . S.fromList

-- | Generate a random pack based on the pack configuration
genPack :: PackConfig -> [CardObj] -> IO (HashSet CardObj)
genPack config cards = do
  if config ^. set == "stx"
    then genStrixhavenPack config cards
    else do
      let setCards = english . filterBySet (config ^. set) . filterDesired $ cards
          base = filterBasicLands Out setCards
          english = S.filter (\c -> c ^. lang == "en")
          fbr r = filterByRarity r base
          foils = S.filter (^. foil) base
      commonWithMaybeFoilCards <-
        commonWithMaybeFoil (config ^. foilChance) (config ^. commons) (fbr Common) foils
      uncommonCards <- gen (config ^. uncommons) (fbr Uncommon)
      pick <- pickRareOrMythic (config ^. mythicChance)
      rareOrMythicCards <- gen (config ^. rareOrMythics) (fbr pick)
      pure $ S.unions [commonWithMaybeFoilCards, uncommonCards, rareOrMythicCards]

-- | Generate a strixhaven pack (has special rules)
genStrixhavenPack :: PackConfig -> [CardObj] -> IO (HashSet CardObj)
genStrixhavenPack config cards = do
  let stxCards = english . filterBySet (config ^. set) . filterDesired $ cards
      baseNoLesson = filterLesson Out . filterBasicLands Out $ stxCards
      lessons = filterLesson In stxCards
      staCards = english . filterBySet "sta" . S.fromList $ cards
      english = S.filter (\c -> c ^. lang == "en")
      fbr r = filterByRarity r baseNoLesson
      foils = S.filter (^. foil) baseNoLesson
  -- Lesson card is picked individually, therefore remove 1 of the common card pick
  commonWithMaybeFoilCards <-
    commonWithMaybeFoil (config ^. foilChance) (config ^. commons - 1) (fbr Common) foils
  uncommonCards <- gen (config ^. uncommons) (fbr Uncommon)
  pick <- pickRareOrMythic (config ^. mythicChance)
  rareOrMythicCards <- gen (config ^. rareOrMythics) (fbr pick)
  lesson <- genByRarity 1 [Common, Rare, Mythic] lessons
  mysticalArchive <- genByRarity 1 [Uncommon .. Mythic] staCards
  pure $ S.unions [commonWithMaybeFoilCards, uncommonCards, rareOrMythicCards, lesson, mysticalArchive]

-- | Generate set of cards filtered by rarity
genByRarity :: Int -> [Rarity] -> HashSet CardObj -> IO (HashSet CardObj)
genByRarity n rarityList cardSet = do
  rarity <- pickRarity rarityList
  cardSet' <- gen n (filterByRarity rarity cardSet)
  pure cardSet'

-- | Generate set of n cards from set
gen :: Int -> HashSet CardObj -> IO (HashSet CardObj)
gen n cards = go n cards S.empty
  where
    go i pool acc
      | i <= 0 || S.null pool = pure acc
      | otherwise = do
        rand <- getStdRandom (randomR (0, S.size pool - 1))
        let card = S.toList pool !! rand
        go (i - 1) (S.delete card pool) (S.insert card acc)
