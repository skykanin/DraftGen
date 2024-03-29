{- |
   Module      : Generate
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Generate packs
-}
module Generate
  ( encodeFile
  , filterBySet
  , genLands
  , genPacks
  , genTokens
  , readCards
  , S.size
  , findCard
  , filterDesired
  )
where

import CLI (Ratio (..))
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.Char (toLower)
import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import Data.Sequence (Seq)
import Data.Sequence qualified as Sq
import System.Random (Random (randomR), getStdRandom)
import Types
  ( BorderColor (..)
  , CardObj (set)
  , FrameEffect (..)
  , PackConfig
  , Rarity (..)
  )
import Types qualified

-- | Read cards from filepath into memory
readCards :: FilePath -> IO (Either String (List CardObj))
readCards = eitherDecodeFileStrict

-- | Filter cards by MTG set predicate
filterBySet :: String -> HashSet CardObj -> HashSet CardObj
filterBySet mtgSet = S.filter (\card -> card.set == mtgSet)

-- | Undesired card layouts
unwantedLayout :: List String
unwantedLayout = ["art_series", "planar", "scheme", "vanguard", "token", "double_faced_token", "emblem"]

-- | No weird frame effects pls
unwantedFrameEffects :: List FrameEffect
unwantedFrameEffects = [Draft, ExtendedArt, Inverted, Showcase]

-- | Search for a card in the a set of cards
findCard :: String -> List CardObj -> Maybe CardObj
findCard query = find matchName . filterDesired
 where
  matchName c = query' `isPrefixOf` cardName
   where
    query' = map toLower query
    cardName = map toLower c.name

-- | Filter out undesired card types
filterDesired :: List CardObj -> List CardObj
filterDesired = filter $ \card ->
  all
    ($ card)
    [ \card -> card.layout `notElem` unwantedLayout
    , \card -> null $ card.frameEffects `intersect` unwantedFrameEffects
    , \card -> not card.variation
    , \card -> not card.reprint
    , \card -> not card.fullArt
    , \card -> not card.promo
    , \card -> card.borderColor /= ColorBorderless
    ]

-- | Filter cards by MTG rarity
filterByRarity :: Rarity -> HashSet CardObj -> HashSet CardObj
filterByRarity rarPred = S.filter (\card -> card.rarity == rarPred)

data Include = In | Out

-- | Filter on MTG type line 'lesson'
filterLesson :: Include -> HashSet CardObj -> HashSet CardObj
filterLesson incl =
  S.filter (p incl . isLesson)
 where
  isLesson card = "Lesson" `isSuffixOf` card.typeLine
  p In = identity
  p Out = not

-- | Filter on MTG basic lands
filterBasicLands :: Include -> HashSet CardObj -> HashSet CardObj
filterBasicLands incl =
  S.filter (\card -> p incl $ checkBasic card && checkLand card)
 where
  checkBasic card = "Basic" `isPrefixOf` card.typeLine
  checkLand card = "Land" `isInfixOf` card.typeLine
  p In = identity
  p Out = not

-- | Pick a rarity to choose from based on the mythic drop chance in the pack configuration
pickRareOrMythic :: Ratio -> IO Rarity
pickRareOrMythic (Ratio numerator denominator) = do
  chance <- getStdRandom (randomR (1, denominator))
  pure $ if chance > numerator then Rare else Mythic

-- Strixhaven rarity weights for lessons and mystical archive cards

stxLessonRarities :: [(Rarity, [Int])]
stxLessonRarities = [(Common, [1, 2, 3]), (Uncommon, [4, 5]), (Rare, [6, 7]), (Mythic, [8])]

stxArchiveRarities :: [(Rarity, [Int])]
stxArchiveRarities = [(Uncommon, [1, 2, 3, 4, 5]), (Rare, [6, 7]), (Mythic, [8])]

data StxCardType = Lesson | Archive

-- | Pick a rarity based on the hardcoded strixhaven rarity weights
pickStxRarity :: StxCardType -> IO Rarity
pickStxRarity cardType = do
  let ratios = case cardType of
        Generate.Lesson -> stxLessonRarities
        Generate.Archive -> stxArchiveRarities
  rarityIdx <- getStdRandom (randomR (1, 8))
  pure
    . fst
    . fromMaybe (error "No matching rarity found in range")
    . find (\(_, range) -> rarityIdx `elem` range)
    $ ratios

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
genPacks :: PackConfig -> List CardObj -> IO (List (Seq CardObj))
genPacks config = replicateM config.amount . genPack config

-- | Return basic lands belonging to the data set
genLands :: PackConfig -> List CardObj -> List (HashSet CardObj)
genLands config = pure . filterBasicLands In . filterBySet config.set . S.fromList

-- | Generate the token cards for a given set
genTokens :: PackConfig -> List CardObj -> List (HashSet CardObj)
genTokens config = pure . filterBySet ('t' : config.set) . S.fromList

-- | Generate a random pack based on the pack configuration
genPack :: PackConfig -> List CardObj -> IO (Seq CardObj)
genPack config cards =
  if config.set == "stx"
    then genStrixhavenPack config cards
    else do
      let setCards = english . filterBySet config.set . S.fromList . filterDesired $ cards
          base = filterBasicLands Out setCards
          english = S.filter (\c -> c.lang == "en")
          fbr r = filterByRarity r base
          foils = S.filter (.foil) base
      commonWithMaybeFoilCards <-
        commonWithMaybeFoil config.foilChance config.commons (fbr Common) foils
      uncommonCards <- gen config.uncommons (fbr Uncommon)
      pick <- pickRareOrMythic config.mythicChance
      rareOrMythicCards <- gen config.rareOrMythics (fbr pick)
      pure $ fromSets [commonWithMaybeFoilCards, uncommonCards, rareOrMythicCards]

fromSets :: List (HashSet a) -> Seq a
fromSets = foldr ((Sq.><) . Sq.fromList . S.toList) Sq.empty

-- | Generate a strixhaven pack (has special rules)
genStrixhavenPack :: PackConfig -> List CardObj -> IO (Seq CardObj)
genStrixhavenPack config cards = do
  let stxCards = english . filterBySet config.set . S.fromList . filterDesired $ cards
      baseNoLesson = filterLesson Out . filterBasicLands Out $ stxCards
      lessons = filterLesson In stxCards
      staCards = english . filterBySet "sta" . S.fromList $ cards
      english = S.filter (\card -> card.lang == "en")
      fbr r = filterByRarity r baseNoLesson
      foils = S.filter (.foil) baseNoLesson
  -- Lesson card is picked individually, therefore remove 1 of the common card pick
  commonWithMaybeFoilCards <-
    commonWithMaybeFoil config.foilChance (config.commons - 1) (fbr Common) foils
  uncommonCards <- gen config.uncommons (fbr Uncommon)
  pick <- pickRareOrMythic config.mythicChance
  rareOrMythicCards <- gen config.rareOrMythics (fbr pick)
  lesson <- genByType 1 Generate.Lesson lessons
  mysticalArchive <- genByType 1 Archive staCards
  pure $ fromSets [commonWithMaybeFoilCards, uncommonCards, rareOrMythicCards, lesson, mysticalArchive]

-- | Generate set of cards filtered by card type
genByType :: Int -> StxCardType -> HashSet CardObj -> IO (HashSet CardObj)
genByType n cardType cardSet = do
  rarity <- pickStxRarity cardType
  gen n (filterByRarity rarity cardSet)

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
