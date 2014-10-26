module Tile where
import Follower
import Object
import Adventure

data Tile = Tile {
  followers::[Follower],
  objects::[Object],
  adventures::[Adventure]
}

defaultTile::Tile
defaultTile = Tile {
  followers=[],
  objects=[],
  adventures = []
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

