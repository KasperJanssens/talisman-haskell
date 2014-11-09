module Board.BoardTest where

import Test.Hspec
import Characters.PlayerCharacter
import Control.Lens
import Board.Board

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

movingSpec::Spec
movingSpec = describe "moving from the crags" $ do
               it "should end us up in  plains4 or fields6" $ do
                 let possibleMoves = calculatePossibleMoves 1 1 0 
                 possibleMoves `shouldMatchList` [Plains4Space plains4,Fields6Space fields6]

