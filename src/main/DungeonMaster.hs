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

chosenPlayers::[ReifiedPrism' Character (Player, SelectTileFunc)]
chosenPlayers = [Prism _Wizard, Prism _OgreChieftain, Prism _Thief]


getSelectedPlayerPosition:: ReifiedPrism' Character Player -> StateT [Character] IO Int
getSelectedPlayerPosition characterPrism= do
    players <- get
    return $ view place $ getPlayer characterPrism players

updatePlayerPosition:: ReifiedPrism' Character Player -> Int -> StateT [Character] IO ()
updatePlayerPosition characterPrism position = do
    players <- get
    let newPlayers = map (over (runPrism characterPrism) (set place position)) players
    put newPlayers

getOtherPlayersInSamePosition::ReifiedPrism' Character (Player, SelectTileFunc) -> StateT [Character] IO [ReifiedPrism' Character (Player, SelectTileFunc)]
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

playRound::[ReifiedPrism' Character Player] -> ([Tile] -> IO Tile) -> StateT [DieRoll] (StateT [Character] IO) ()
playRound currentPlayers selectTileFunc = foldM_
      (\_ characterPrism -> do
         dieRoll <- nextRoll
         lift $ handlePlayerMove selectTileFunc dieRoll characterPrism
         return ())
    () currentPlayers


handlePlayerMove::([Tile] -> IO Tile) -> Int -> ReifiedPrism' Character Player-> StateT [Character] IO ()
handlePlayerMove selectTileFunc dieRoll characterPrism = do
    selectedPlayerPosition <- getSelectedPlayerPosition characterPrism
    let options = getMovingOptions dieRoll selectedPlayerPosition
    let lookUpTilePrism = head options
    selectedTile <- liftIO $ selectTileFunc $ mapMaybe (preview $ runPrism lookUpTilePrism)spaces
    let newTileNumber = view tileNumber selectedTile
    updatePlayerPosition characterPrism newTileNumber




