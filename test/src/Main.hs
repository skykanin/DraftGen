{- |
   Module      : Main
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable

 Entrypoint for tests
-}
module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Generate (filterDesired, readCards)
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

basic :: TopSpec
basic = describe "Unit tests" $ do
  it "filterDesired filters out undesired card types" testFilterDesired

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions basic
