module Board.BoardTest where

import Test.Hspec
import Characters.PlayerCharacter
import Control.Lens
import Board.Board
import Data.Maybe

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
                 let viewSpaceFunctions = snd . unzip $ getMovingOptions 1 23
                 let selectedTiles = foldl (\acc viewFunc -> acc ++ (catMaybes $ map viewFunc spaces)) [] viewSpaceFunctions
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [22,24]
               it "die roll 2 should end us up in  chapel or woods3" $ do
                 let viewSpaceFunctions = snd . unzip $ getMovingOptions 2 23
                 let selectedTiles = foldl (\acc viewFunc -> acc ++ (catMaybes $ map viewFunc spaces)) [] viewSpaceFunctions
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [21,1]
               it "die roll 3 should end us up in  fields5 or hills1" $ do
                 let viewSpaceFunctions = snd . unzip $ getMovingOptions 3 23
                 let selectedTiles = foldl (\acc viewFunc -> acc ++ (catMaybes $ map viewFunc spaces)) [] viewSpaceFunctions
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [20,2]
               it "die roll 4 should end us up in  sentinel or city" $ do
                 let viewSpaceFunctions = snd . unzip $ getMovingOptions 4 23
                 let selectedTiles = foldl (\acc viewFunc -> acc ++ (catMaybes $ map viewFunc spaces)) [] viewSpaceFunctions
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [19,3]
               it "die roll 5 should end us up in  woods1 or fiels4" $ do
                 let viewSpaceFunctions = snd . unzip $ getMovingOptions 5 23
                 let selectedTiles = foldl (\acc viewFunc -> acc ++ (catMaybes $ map viewFunc spaces)) [] viewSpaceFunctions
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [18,4]
               it "die roll 6 should end us up in  graveyard or hills2" $ do
                 let viewSpaceFunctions = snd . unzip $ getMovingOptions 6 23
                 let selectedTiles = foldl (\acc viewFunc -> acc ++ (catMaybes $ map viewFunc spaces)) [] viewSpaceFunctions
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [17,5]


