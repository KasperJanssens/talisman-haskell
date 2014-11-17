module Board.BoardTest where

import Test.Hspec
import Characters.PlayerCharacter
import Control.Lens
import Control.Lens.Reified
import Board.Board
import Data.Maybe

placingSpec::Spec
placingSpec = describe "placing the characters" $ do
         it "should place mr ogre in the crags " $ do
            let currentPlace = view (singular $ each._OgreChieftain.place) allPlayers
            currentPlace `shouldBe` 23
         it "should place mr wizard in the graveyard" $ do
             let currentPlace = view (singular $ each._Wizard.place) allPlayers
             currentPlace `shouldBe` 5
         it "should place mr thief in the city" $ do
             let currentPlace =  view (singular $ each._Thief.place) allPlayers
             currentPlace `shouldBe` 19

lookupTiles::[ReifiedPrism' Space Tile] -> [Tile]
lookupTiles = foldl (\acc prism -> acc ++ (mapMaybe (preview $ runPrism prism) spaces)) []

movingSpec::Spec
movingSpec = describe "moving from the crags" $ do
               it "die roll 1 should end us up in  plains4 or fields6" $ do
                 let viewSpacePrisms = getMovingOptions 1 23
                 let selectedTiles = lookupTiles viewSpacePrisms
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [22,24]
               it "die roll 2 should end us up in  chapel or woods3" $ do
                 let viewSpacePrisms = getMovingOptions 2 23
                 let selectedTiles = lookupTiles viewSpacePrisms
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [21,1]
               it "die roll 3 should end us up in  fields5 or hills1" $ do
                 let viewSpacePrisms = getMovingOptions 3 23
                 let selectedTiles = lookupTiles viewSpacePrisms
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [20,2]
               it "die roll 4 should end us up in  sentinel or city" $ do
                 let viewSpacePrisms = getMovingOptions 4 23
                 let selectedTiles = lookupTiles viewSpacePrisms
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [19,3]
               it "die roll 5 should end us up in  woods1 or fiels4" $ do
                 let viewSpacePrisms = getMovingOptions 5 23
                 let selectedTiles = lookupTiles viewSpacePrisms
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [18,4]
               it "die roll 6 should end us up in  graveyard or hills2" $ do
                 let viewSpacePrisms = getMovingOptions 6 23
                 let selectedTiles = lookupTiles viewSpacePrisms
                 let selectedTileNumbers = map (view tileNumber) selectedTiles
                 length selectedTiles `shouldBe` 2
                 selectedTileNumbers `shouldMatchList` [17,5]


