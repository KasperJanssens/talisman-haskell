module Board.Tile where
import Characters.Follower
import Object
import Adventure
import Characters.PlayerCharacter

data Tile = Tile {
  freeFollowers::[Follower],
  freeObjects::[Object],
  adventures::[Adventure],
  players::[Player]
}

defaultTile::Tile
defaultTile = Tile {
  freeFollowers=[],
  freeObjects=[],
  adventures = [],
  players=[]
}

newtype Fields = Fields Tile
fields::Fields
fields = Fields defaultTile

newtype Forest = Forest Tile
forest::Forest
forest=Forest defaultTile

newtype Ruins = Ruins Tile
ruins::Ruins
ruins = Ruins defaultTile

newtype Tavern = Tavern Tile
tavern::Tavern
tavern = Tavern defaultTile

newtype Plains = Plains Tile
plains::Plains
plains = Plains defaultTile

newtype Woods = Woods Tile
woods::Woods
woods=Woods defaultTile

newtype Hills = Hills Tile
hills::Hills
hills = Hills defaultTile

newtype City = City Tile
city::City
city = City defaultTile

newtype Crags = Crags Tile
crags::Crags
crags = Crags defaultTile

newtype Chapel = Chapel Tile
chapel::Chapel
chapel = Chapel defaultTile

newtype Graveyard = Graveyard Tile
graveyard::Graveyard
graveyard = Graveyard defaultTile

newtype Sentinel = Sentinel Tile
sentinel :: Sentinel
sentinel = Sentinel defaultTile

newtype Village = Village Tile
village :: Village
village = Village defaultTile


