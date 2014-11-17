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

nextRoll:: (Monad m) => StateT [DieRoll] m DieRoll
nextRoll = do
    rolls <- get
    let nextOne = head rolls
    put $ tail rolls
    return nextOne

type DieRoll = Int

chosenPlayers::[ReifiedPrism' Character Player]
chosenPlayers = [Prism _Wizard, Prism _OgreChieftain, Prism _Thief]

getPlayer::ReifiedPrism' Character Player -> [Character] -> Player
getPlayer characterPrism chars = head $ mapMaybe (preview $ runPrism characterPrism) chars

getCharacter::ReifiedPrism' Character Player -> [Character] -> Character
getCharacter prism chars = review (runPrism prism) $ getPlayer prism chars

--getCharacterPrism::Character -> [ReifiedPrism' Character Player] -> ReifiedPrism' Character Player
--getCharacterPrism char prisms =  foldl (\elem )


--foldl (\acc elem -> if isJust $ preview (runPrism elem) char ) [] prisms

getSelectedPlayerPosition:: ReifiedPrism' Character Player -> StateT [Character] IO Int
getSelectedPlayerPosition characterPrism= do
    players <- get
    return $ view place $ getPlayer characterPrism players

updatePlayerPosition:: ReifiedPrism' Character Player -> Int -> StateT [Character] IO ()
updatePlayerPosition characterPrism position = do
    players <- get
    let newPlayers = map (over (runPrism characterPrism) (set place position)) players
    put newPlayers


playRound::[ReifiedPrism' Character Player] -> ([Tile] -> IO Tile) -> StateT [DieRoll] (StateT [Character] IO) ()
playRound currentPlayers selectTileFunc = foldM_
      (\_ characterPrism -> do
         dieRoll <- nextRoll
         _newPlace <- lift $ handlePlayerMove selectTileFunc dieRoll characterPrism
         currentChar <- lift $ do
             currentPlayers <- get
             return $ getCharacter characterPrism currentPlayers
         return ())
    () currentPlayers


handlePlayerMove::([Tile] -> IO Tile) -> Int -> ReifiedPrism' Character Player-> StateT [Character] IO Int
handlePlayerMove selectTileFunc dieRoll characterPrism = do
    selectedPlayerPosition <- getSelectedPlayerPosition characterPrism
    let options = getMovingOptions dieRoll selectedPlayerPosition
    let lookUpTilePrism = head options
    selectedTile <- liftIO $ selectTileFunc $ mapMaybe (preview $ runPrism lookUpTilePrism)spaces
    let newTileNumber = view tileNumber selectedTile
    updatePlayerPosition characterPrism newTileNumber
    return newTileNumber


