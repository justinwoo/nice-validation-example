module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (fold, foldl, intercalate)
import Data.List.Types (NonEmptyList)
import Data.Validation.Semigroup (V, invalid, unV)

type FormData =
  { appleColor :: String
  , bananaColor :: String
  , carrotColor :: String
  }

type MyValidated a = V (NonEmptyList String) a

appleIsRed :: String -> MyValidated String
appleIsRed s =
  if s == "red"
     -- creates our MyValidated with the right side Valid String
     then pure s
     -- creates our MyValidated with the left side InValid (NonEmptyList String)
     else invalid $ pure "apple wasn't red"

bananaIsNotGreen :: String -> MyValidated String
bananaIsNotGreen s =
  if s /= "green"
     then pure s
     else invalid $ pure "banana was green"

testData :: FormData
testData =
  { appleColor: "red"
  , bananaColor: "yellow"
  , carrotColor: "orange"
  }

testMyValidated :: MyValidated FormData
testMyValidated =
  { appleColor: _, bananaColor: _, carrotColor: testData.carrotColor }
  <$> appleIsRed testData.appleColor
  <*> bananaIsNotGreen testData.bananaColor

printMyValidated :: MyValidated FormData -> Eff _ Unit
printMyValidated = unV
  (\errors -> log $ "got errors: " <> intercalate ", " errors)
  (\formData ->
    log
      $ "the apples were "
      <> formData.appleColor
      <> " and the bananas "
      <> formData.bananaColor
  )

errorMyValidated1 :: MyValidated FormData
errorMyValidated1 =
  { appleColor: _, bananaColor: _, carrotColor: testData.carrotColor }
  <$> appleIsRed "red"
  <*> bananaIsNotGreen "green"

errorMyValidated2 :: MyValidated FormData
errorMyValidated2 =
  { appleColor: _, bananaColor: _, carrotColor: testData.carrotColor }
  <$> appleIsRed "yellow"
  <*> bananaIsNotGreen "green"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  printMyValidated testMyValidated
  -- output:
  -- the apples were red and the bananas yellow

  printMyValidated errorMyValidated1
  printMyValidated errorMyValidated2
  -- output:
  -- got errors: banana was green
  -- got errors: apple wasn't red, banana was green

