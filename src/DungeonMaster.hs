module DungeonMaster where
import Characters.PlayerCharacter

import Control.Monad.Trans.State
import System.Random

type GeneratorState = State StdGen

rollDie :: GeneratorState Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value



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

