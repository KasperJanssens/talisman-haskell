module DungeonMaster where
import Characters.PlayerCharacter

import Control.Monad.Trans.State
import System.Random
import Board.Board
import Control.Lens
import Data.Maybe

type GeneratorState = State StdGen

rollDie :: GeneratorState Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value

rollLoadedDieAlways1::GeneratorState Int
rollLoadedDieAlways1 = return 1

chosenPlayers::[((Player -> Player) -> Character -> Character, Character -> Maybe Player)]
chosenPlayers= [(over _Wizard, preview _Wizard)
                , (over _OgreChieftain, preview _OgreChieftain)
                , (over _Thief, preview _Thief)
                ]

--playRound:: IO()
--playRound = do
    
handlePlayerMove::Int -> (Character -> Maybe Player) -> ((Player -> Player) -> Character -> Character) -> ()
handlePlayerMove  dieRoll lookUpPlayerFunc updatePlayerFunc = 
    let selectedPlayer = head $ catMaybes $ map lookUpPlayerFunc allPlayers
        currentSpace = (view place) selectedPlayer
        options = getMovingOptions dieRoll currentSpace
        lookUpTileFunc = snd $ head options
        selectedTile = head $ catMaybes $ map lookUpTileFunc spaces
        newTileNumber = view tileNumber selectedTile
    in  ()
