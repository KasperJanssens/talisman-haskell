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
} deriving (Eq, Ord)

makeLenses ''Tile

defaultTile::Tile
defaultTile = Tile {
  _freeFollowers=[],
  _freeObjects=[],
  _adventures = [],
  _players=[]
}

newtype Fields = Fields Tile deriving (Eq, Ord)
fields::Fields
fields = Fields defaultTile

newtype Forest = Forest Tile deriving (Eq, Ord)
forest::Forest
forest=Forest defaultTile

newtype Ruins = Ruins Tile deriving (Eq, Ord)
ruins::Ruins
ruins = Ruins defaultTile

newtype Tavern = Tavern Tile deriving (Eq, Ord)
tavern::Tavern
tavern = Tavern defaultTile

newtype Plains = Plains Tile deriving (Eq, Ord)
plains::Plains
plains = Plains defaultTile

newtype Woods = Woods Tile deriving (Eq, Ord)
woods::Woods
woods=Woods defaultTile

newtype Hills = Hills Tile deriving (Eq, Ord)
hills::Hills
hills = Hills defaultTile

newtype City = City Tile deriving (Eq, Ord)
city::City
city = City defaultTile

newtype Crags = Crags Tile deriving (Eq, Ord)
crags::Crags
crags = Crags defaultTile

newtype Chapel = Chapel Tile deriving (Eq, Ord)
chapel::Chapel
chapel = Chapel defaultTile

newtype Graveyard = Graveyard Tile deriving (Eq, Ord)
graveyard::Graveyard
graveyard = Graveyard defaultTile

newtype Sentinel = Sentinel Tile deriving (Eq, Ord)
sentinel :: Sentinel
sentinel = Sentinel defaultTile

newtype Village = Village Tile deriving (Eq, Ord)
village :: Village
village = Village defaultTile


