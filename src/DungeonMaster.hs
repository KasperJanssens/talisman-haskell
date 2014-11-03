module DungeonMaster where
import Characters.PlayerCharacter


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

