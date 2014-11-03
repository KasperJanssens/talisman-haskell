{-# LANGUAGE TemplateHaskell #-}
module Board.Tile where
import Characters.Follower
import Object
import Adventure
import Characters.PlayerCharacter
import Control.Lens

data Tile = Tile {
  _freeFollowers::[Follower],
  _freeObjects::[Object],
  _adventures::[Adventure],
  _players::[Character]
} 

makeLenses ''Tile

defaultTile::Tile
defaultTile = Tile {
  _freeFollowers=[],
  _freeObjects=[],
  _adventures = [],
  _players=[]
}

