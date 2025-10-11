{- |
   Module      : Main
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Entrypoint for tests
-}
module Main where

import CLI
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson qualified as Json
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import File qualified
import Generate (filterDesired, readCards)
import Generate qualified
import System.FilePath
import Test.Sandwich
import Types

jsonPath = "test/json"

testFilterDesired :: (HasCallStack, MonadIO m, MonadThrow m) => m ()
testFilterDesired =
  for_ filterExamples $ \(expected, filepath) -> do
    eitherCards <- liftIO $ readCards filepath
    testFilters expected eitherCards

filterExamples :: List (Int, FilePath)
filterExamples =
  over _2 (combine jsonPath)
    <$> [ (1, "Rivendell.json")
        , (1, "Minas Tirith.json")
        , (1, "Bilbo, Retired Burglar.json")
        , (1, "Gandalf the White.json")
        , (1, "Sauron, the Dark Lord.json")
        ]

testFilters :: (MonadIO m, MonadThrow m) => Int -> Either String (HashSet CardObj) -> m ()
testFilters expectedLength eitherCards =
  case eitherCards of
    Left err -> unexpectedError err
    Right cards -> do
      let result = filterDesired cards
      HS.size result `shouldBe` expectedLength
      let [cardObj] = HS.toList result
      cardObj.promo `shouldBe` False
      cardObj.reprint `shouldBe` False
      cardObj.fullArt `shouldBe` False
      cardObj.variation `shouldBe` False
      cardObj.borderColor `shouldNotBe` ColorBorderless

encodeDecodeIsInverse :: (MonadIO m, MonadThrow m, Json.FromJSON a, Json.ToJSON a, Eq a, Show a) => a -> m ()
encodeDecodeIsInverse v = Json.decode (Json.encode v) `shouldBe` Just v

testCardFaceInverse :: (MonadIO m, MonadThrow m) => m ()
testCardFaceInverse = encodeDecodeIsInverse CardFace {name = "Back", imageUris = Nothing}

testFrameEffectInverse :: (MonadIO m, MonadThrow m) => m ()
testFrameEffectInverse = encodeDecodeIsInverse CompassLandDfc

testBorderColorInverse :: (MonadIO m, MonadThrow m) => m ()
testBorderColorInverse = encodeDecodeIsInverse ColorBlack

-- | Simulate running DraftGen from the command line
-- This function generates packs, encodes and writes them to the file system.
simulateMain :: (MonadIO m, MonadThrow m) => m ()
simulateMain = runExceptT (File.run config) *> shouldBe True True
 where
  config =
    PackConfig
      { amount = 6
      , set = "fin"
      , commons = 10
      , uncommons = 3
      , rareOrMythics = 1
      , mythicChance = Ratio 1 8
      , foilChance = Ratio 1 45
      }

generatePack :: MonadIO m => PackConfig -> ExceptT String m (Seq CardObj)
generatePack config = do
  cards <- File.getFromCache config.set
  liftIO $ Generate.genPack config cards

generatesValidPack :: (MonadIO m, MonadThrow m) => m ()
generatesValidPack = do
  packRes <- runExceptT $ generatePack config
  case packRes of
    Left err -> expectationFailure err
    Right pack -> Seq.length pack `shouldBe` (config.commons + config.uncommons + config.rareOrMythics)
 where
  config =
    PackConfig
      { amount = 6
      , set = "om1"
      , commons = 10
      , uncommons = 3
      , rareOrMythics = 1
      , mythicChance = Ratio 1 8
      , foilChance = Ratio 1 45
      }

basic :: TopSpec
basic = describe "Unit tests" $ do
  it "filterDesired filters out undesired card types" testFilterDesired
  it "cardFace encode/decode are inverses" testCardFaceInverse
  it "frameEffect encode/decode are inverses" testFrameEffectInverse
  it "borderColor encode/decode are inverses" testBorderColorInverse
  it "generates a valid pack with the expected contents" generatesValidPack
  it "generates packs without throwing exceptions" simulateMain

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions basic
