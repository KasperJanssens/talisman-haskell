module DungeonMaster where
import Characters.PlayerCharacter

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.IO.Class
import Board.Board
import Control.Lens
import Control.Lens.Reified
import Data.Maybe
import Data.List as List

nextRoll:: (Monad m) => StateT [DieRoll] m DieRoll
nextRoll = do
    rolls <- get
    let nextOne = head rolls
    put $ tail rolls
    return nextOne

type DieRoll = Int

chosenPlayers::[ReifiedPrism' Character Player]
chosenPlayers = [Prism _Wizard, Prism _OgreChieftain, Prism _Thief]


getSelectedPlayerPosition:: ReifiedPrism' Character Player -> StateT [Character] IO Int
getSelectedPlayerPosition characterPrism= do
    characters <- get
    return $ view place $ getPlayer characterPrism characters

updatePlayerPosition:: ReifiedPrism' Character Player -> Int -> StateT [Character] IO ()
updatePlayerPosition characterPrism position = do
    characters <- get
    let newPlayers = map (over (runPrism characterPrism) (set place position )) characters
    put newPlayers

getOtherPlayersInSamePosition::ReifiedPrism' Character Player -> StateT [Character] IO [ReifiedPrism' Character Player]
getOtherPlayersInSamePosition curPlayerPrism = do
    currentPlayerPosition <- getSelectedPlayerPosition curPlayerPrism
    currentPlayers <- get
    let currentChar  = getCharacter curPlayerPrism currentPlayers
    let otherPlayers = List.delete currentChar currentPlayers
    let otherPrisms  = List.map getPrism otherPlayers
    foldM (\acc prism -> do
                           otherPlayerPosition <- getSelectedPlayerPosition prism
                           case () of
                               _ | otherPlayerPosition == currentPlayerPosition -> return $ prism:acc
                                 | otherwise                                    -> return acc
                           )
          [] otherPrisms

playRound::[ReifiedPrism' Character Player] -> StateT [DieRoll] (StateT [Character] IO) ()
playRound currentPlayers = foldM_
      (\_ characterPrism -> do
         dieRoll <- nextRoll
         lift $ handlePlayerMove dieRoll characterPrism
         return ())
    () currentPlayers


handlePlayerMove::Int -> ReifiedPrism' Character Player-> StateT [Character] IO ()
handlePlayerMove dieRoll characterPrism = do
    characters <- get
    let character = getPlayer characterPrism characters
    let selectTile = character ^. selectTileFunc
    selectedPlayerPosition <- getSelectedPlayerPosition characterPrism
    let reachableTiles = getMovingOptions dieRoll selectedPlayerPosition
    selectedTile <- liftIO $  selectTile reachableTiles
    let newTileNumber = view tileNumber selectedTile
    updatePlayerPosition characterPrism newTileNumber




