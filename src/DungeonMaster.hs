module DungeonMaster where
import Characters.PlayerCharacter

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.IO.Class
import Board.Board
import Control.Lens
import Data.Maybe

nextRoll:: (Monad m) => StateT [DieRoll] m DieRoll
nextRoll = do
    rolls <- get
    let nextOne = head rolls
    put $ tail rolls
    return nextOne

type DieRoll = Int

type CharacterHandle = ((Player -> Player) -> Character -> Character, Character -> Maybe Player)

chosenPlayers::[CharacterHandle]
chosenPlayers= [(over _Wizard, preview _Wizard)
                , (over _OgreChieftain, preview _OgreChieftain)
                , (over _Thief, preview _Thief)
                ]

getSelectedPlayerPosition:: (Character -> Maybe Player) -> StateT [Character] IO Int
getSelectedPlayerPosition lookUpPlayerFunc= do
    players <- get
    return $ view place $ head $ mapMaybe lookUpPlayerFunc players

updatePlayerPosition:: ((Player -> Player) -> Character -> Character) -> Int -> StateT [Character] IO ()
updatePlayerPosition updatePlayerFunc position = do
    players <- get
    let newPlayers = map (updatePlayerFunc (set place position)) players
    put newPlayers


playRound::[CharacterHandle] -> ([Tile] -> IO Tile) -> StateT [DieRoll] (StateT [Character] IO) ()
playRound currentPlayers selectTileFunc = foldM_
      (\_ characterHandle -> do
         dieRoll <- nextRoll
         lift $ handlePlayerMove selectTileFunc dieRoll characterHandle)
    () currentPlayers


handlePlayerMove::([Tile] -> IO Tile) -> Int -> CharacterHandle -> StateT [Character] IO ()
handlePlayerMove selectTileFunc dieRoll (updatePlayerFunc,lookUpPlayerFunc) = do
    selectedPlayerPosition <- getSelectedPlayerPosition lookUpPlayerFunc
    let options = getMovingOptions dieRoll selectedPlayerPosition
    let lookUpTileFunc = snd $ head options
    selectedTile <- liftIO $ selectTileFunc $ mapMaybe lookUpTileFunc spaces
    let newTileNumber = view tileNumber selectedTile
    updatePlayerPosition updatePlayerFunc newTileNumber


