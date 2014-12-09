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

fightPlayers:: ReifiedPrism' Character Player -> StateT [Character] IO ()
fightPlayers charPrism = do
  otherPlayersOnPos <- getOtherPlayersInSamePosition charPrism
  let selectFightFunc = charPrism ^. selectPlayerFunc
  chosenFight <- liftIO $ selectFightFunc otherPlayersOnPos
  return ()
  --maybe

playRound::[ReifiedPrism' Character Player] -> StateT [DieRoll] (StateT [Character] IO) ()
playRound currentPlayers = foldM_
      (\_ characterPrism -> do
         dieRoll <- nextRoll
         lift $ movePlayer dieRoll characterPrism
         fightPlayers characterPrism

         return ())
    () currentPlayers


movePlayer::Int -> ReifiedPrism' Character Player-> StateT [Character] IO ()
movePlayer dieRoll characterPrism = do
    characters <- get
    let character = getPlayer characterPrism characters
    let selectTile = character ^. selectTileFunc
    selectedPlayerPosition <- getSelectedPlayerPosition characterPrism
    let reachableTiles = getMovingOptions dieRoll selectedPlayerPosition
    selectedTile <- liftIO $  selectTile reachableTiles
    let newTileNumber = selectedTile ^. tileNumber
    updatePlayerPosition characterPrism newTileNumber




