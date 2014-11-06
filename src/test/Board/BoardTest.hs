module Board.BoardTest where

import Test.Hspec
import Characters.PlayerCharacter
import Control.Lens

placingSpec::Spec
placingSpec = describe "placing the characters" $ do
         it "should place mr ogre in the crags " $ do
            let tileNumber = view (singular $ each._OgreChieftain.place) allPlayers
            tileNumber `shouldBe` 23
         it "should place mr wizard in the graveyard" $ do
             let tileNumber = view (singular $ each._Wizard.place) allPlayers
             tileNumber `shouldBe` 5
         it "should place mr thief in the city" $ do
             let tileNumber =  view (singular $ each._Thief.place) allPlayers
             tileNumber `shouldBe` 19



{-findingSpec::Spec
findingSpec = describe "find the placed characters back" $ do
                it "should find the wizard" $ do
                   let (_,newBoard) = execState (place charToPlace) spaces
                   find _Wizard newBoard `shouldBe` 7-}