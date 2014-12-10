module LensTest where

import Characters.PlayerCharacter
import Test.Hspec
import Control.Lens

--toTest
lensSpec::Spec
lensSpec =do $
 describe "to" $ do
  it "is the way you can use functions from your data type to one of it's elements as getters -> combine functions with prisms" $ do
    wizard ^. to __strength `shouldBe` 2
 describe "views" $ do
  it "is the way to apply a function on the result of a getter" $ do
    views wizard object length `shouldBe` 0
