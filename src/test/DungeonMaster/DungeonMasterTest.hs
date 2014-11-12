module DungeonMaster.DungeonMasterTest where

import Test.Hspec
import Control.Monad.State
import System.Random
import Characters.PlayerCharacter
import Control.Lens
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
              --  let generator = mkStdGen 0
                --let dieRolls = generate10kDieRolls 10000 generator
                let dieRolls = [6,6,4]
                let newPlayers =  execState (runStateT DungeonMaster.playRound dieRolls) allPlayers
                let ogresPlace = view (singular $ each._OgreChieftain.place) newPlayers
                ogresPlace `shouldBe` 17
                let wizardsPlace = view (singular $ each._Wizard.place) newPlayers
                wizardsPlace `shouldBe` 23
                let thiefsPlace = view (singular $ each._Thief.place) newPlayers
                thiefsPlace `shouldBe` 15

                
