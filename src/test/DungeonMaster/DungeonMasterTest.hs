module DungeonMaster.DungeonMasterTest where

import Test.Hspec
import Control.Monad.State
import System.Random
import Characters.PlayerCharacter
import Control.Lens
import Control.Applicative
import qualified DungeonMaster as DungeonMaster


rollDie :: (Monad m) => StateT StdGen m Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value

generate10kDieRolls::Int -> StdGen -> [Int]
generate10kDieRolls numberOfDieRolls generator = evalState ( replicateM numberOfDieRolls $
    do
        rollDie
    ) generator

roundSpec::Spec
roundSpec= describe "Dungeon master" $ do
            it "should be able to play a round" $ do
                let dieRolls = [6,6,4]
                newPlayers <- execStateT (runStateT (DungeonMaster.playRound DungeonMaster.chosenPlayers) dieRolls) allPlayers
                let ogresPlace = view (singular $ each._OgreChieftain.place) newPlayers
                ogresPlace `shouldBe` 17
                let wizardsPlace = view (singular $ each._Wizard.place) newPlayers
                wizardsPlace `shouldBe` 23
                let thiefsPlace = view (singular $ each._Thief.place) newPlayers
                thiefsPlace `shouldBe` 15

playersInPosition::Spec
playersInPosition = describe "Dungeon Master" $ do
                      it "should find no players on the same position in the begin-state" $ do
                        otherPlayerPrisms <- evalStateT (DungeonMaster.getOtherPlayersInSamePosition $ Prism _Thief) allPlayers
                        length otherPlayerPrisms `shouldBe` 0

                      it "should find Wizard on the same place" $ do
                        let thiefsPlace = view (singular $ each._Thief.place) allPlayers
                        let wizardOnThiefsPlace = over (singular $ each._Wizard.place) (\_ -> thiefsPlace) allPlayers

                        otherPlayerPrisms <- evalStateT (DungeonMaster.getOtherPlayersInSamePosition $ Prism _Thief) wizardOnThiefsPlace

                        length otherPlayerPrisms `shouldBe` 1
                        let actualCharacter = getCharacter (head otherPlayerPrisms) wizardOnThiefsPlace
                        let expectedCharacter = getCharacter (Prism _Wizard) wizardOnThiefsPlace
                        actualCharacter `shouldBe` expectedCharacter


                      it "should find Wizard and OgreChieftain on the same place" $ do
                        let thiefsPlace = view (singular $ each._Thief.place) allPlayers
                        let wizardOnThiefsPlace = over (singular $ each._Wizard.place) (\_ -> thiefsPlace) allPlayers
                        let ogreAndWizardOnThiefsPlace = over (singular $ each._OgreChieftain.place) (\_ -> thiefsPlace) wizardOnThiefsPlace

                        otherPlayerPrisms <- evalStateT (DungeonMaster.getOtherPlayersInSamePosition $ Prism _Thief) ogreAndWizardOnThiefsPlace

                        length otherPlayerPrisms `shouldBe` 2
                        let actualCharacters = map (flip getCharacter $ ogreAndWizardOnThiefsPlace) otherPlayerPrisms
                        let expectedCharacters = map (flip getCharacter $ ogreAndWizardOnThiefsPlace) [Prism _Wizard, Prism _OgreChieftain]
                        actualCharacters `shouldMatchList` expectedCharacters
