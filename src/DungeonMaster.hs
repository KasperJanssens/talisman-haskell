module DungeonMaster where
import Characters.PlayerCharacter

import Control.Monad.Trans.State
import System.Random
import Board.Board

type GeneratorState = State StdGen

rollDie :: GeneratorState Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value

rollLoadedDieAlways1::GeneratorState Int
rollLoadedDieAlways1 = return 1
       
chooseSpace:: [Space] -> IO Space
chooseSpace possibleSpaces = return $ head possibleSpaces


selectPlayers:: IO [Character]
selectPlayers = return allPlayers


{-putPlayersOnBoard :: IO Board.Board
putPlayersOnBoard = do
    let board = Board.createBoard
    liftM (Board.addPlayer board) selectPlayers

main::IO ()
main = do
    _ <- putPlayersOnBoard
    return ()
-}

