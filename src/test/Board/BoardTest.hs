module Board.BoardTest where

import Board.Board
import Test.Hspec
import Control.Monad.State
import Characters.PlayerCharacter
import Control.Lens

placingSpec::Spec
placingSpec = describe "placing the characters" $ do
         it "should place mr ogre in the crags " $ do
            let charToPlace = OgreChieftain ogreChieftain
            let expectedCrag = over players ((:) charToPlace) crags
            let result = execState (do
                                   place charToPlace)
                      spaces
            result `shouldContain` [CragsSpace  expectedCrag]
         it "should place mr wizard in the graveyard" $ do
             let charToPlace = Wizard wizard
             let expectedGraveyard = over players ((:) charToPlace) graveyard
             let result = execState (do
                                   place charToPlace)
                      spaces
             result `shouldContain` [GraveyardSpace  expectedGraveyard]
         it "should place mr thief in the city" $ do
             let charToPlace = Thief thief
             let expectedCity =over players ((:) charToPlace) city
             let result = execState (do
                                   place charToPlace)
                      spaces
             result `shouldContain` [CitySpace  expectedCity]

{-findingSpec::Spec
findingSpec = describe "find the placed characters back" $ do
                it "should find the wizard" $ do
                   let (_,newBoard) = execState (place charToPlace) spaces
                   find _Wizard newBoard `shouldBe` 7-}