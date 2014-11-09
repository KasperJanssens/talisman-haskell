module Board.BoardTest where

import Test.Hspec
import Characters.PlayerCharacter
import Control.Lens
import Board.Board

placingSpec::Spec
placingSpec = describe "placing the characters" $ do
         it "should place mr ogre in the crags " $ do
            let number = view (singular $ each._OgreChieftain.place) allPlayers
            number `shouldBe` 23
         it "should place mr wizard in the graveyard" $ do
             let number = view (singular $ each._Wizard.place) allPlayers
             number `shouldBe` 5
         it "should place mr thief in the city" $ do
             let number =  view (singular $ each._Thief.place) allPlayers
             number `shouldBe` 19

movingSpec::Spec
movingSpec = describe "moving from the crags" $ do
               it "die roll 1 should end us up in  plains4 or fields6" $ do
                 let possibleMoves = getMovingOptions 1 23
                 possibleMoves `shouldMatchList` [Plains4Space plains4,Fields6Space fields6]
               it "die roll 2 should end us up in  chapel or woods3" $ do
                 let possibleMoves = getMovingOptions 2 23
                 possibleMoves `shouldMatchList` [ChapelSpace chapel,Woods3Space woods3]
               it "die roll 3 should end us up in  fields5 or hills1" $ do
                 let possibleMoves = getMovingOptions 3 23
                 possibleMoves `shouldMatchList` [Fields5Space fields5, Hills1Space hills1]
               it "die roll 4 should end us up at sentinel or city" $ do
                 let possibleMoves = getMovingOptions 4 23
                 possibleMoves `shouldMatchList` [SentinelSpace sentinel, CitySpace city]
               it "die roll 5 should end us up in  woods1 or fields4" $ do
                 let possibleMoves = getMovingOptions 5 23
                 possibleMoves `shouldMatchList` [Woods1Space woods1, Fields4Space fields4]
               it "die roll 6 should end us up in  graveyard or hills2" $ do
                 let possibleMoves = getMovingOptions 6 23
                 possibleMoves `shouldMatchList` [GraveyardSpace graveyard, Hills2Space hills2]


