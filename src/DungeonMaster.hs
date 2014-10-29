module DungeonMaster where
import Characters.PlayerCharacter
import qualified Board.Board as Board
import Control.Monad


selectPlayers:: IO [Character]
selectPlayers = return [Wizard wizard, Thief thief, OgreChieftain ogreChieftain]

{-putPlayersOnBoard :: IO Board.Board
putPlayersOnBoard = do
    let board = Board.createBoard
    liftM (Board.addPlayer board) selectPlayers

main::IO ()
main = do
    _ <- putPlayersOnBoard
    return ()
-}

