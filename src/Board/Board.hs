{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Board.Board where
import Control.Lens
import Characters.Follower
import Adventure
import Object

data Tile = Tile {
  _tileNumber::Int,
  _freeFollowers::[Follower],
  _freeObjects::[Object],
  _adventures::[Adventure]
} deriving (Eq, Ord, Show)

makeLenses ''Tile

defaultTile::Int -> Tile
defaultTile number =  Tile {
  _tileNumber = number,
  _freeFollowers=[],
  _freeObjects=[],
  _adventures = []
}


data Space = FieldsSpace Tile
  | ForestSpace Tile
  | RuinsSpace Tile
  | TavernSpace Tile
  | PlainsSpace Tile
  | WoodsSpace Tile
  | HillsSpace Tile
  | CitySpace Tile
  | ChapelSpace Tile
  | SentinelSpace Tile
  | GraveyardSpace Tile
  | VillageSpace Tile
  | CragsSpace Tile deriving (Eq, Ord, Show)

makePrisms ''Space


{-createBoardGraph::Graph.Graph
createBoardGraph = Graph.buildG (firstSpaceNumber, lastSpaceNumber) $ zip [firstSpaceNumber..lastSpaceNumber] $ [firstSpaceNumber+1 .. lastSpaceNumber ] ++[firstSpaceNumber]
                   where firstSpaceNumber = 1
                         lastSpaceNumber = length spaces

goRight::Int -> Graph.Graph -> Int -> Int
goRight steps g startNode =  head $ drop steps $ Graph.reachable g startNode

goLeft::Int -> Graph.Graph -> Int -> Int
goLeft steps g startNode =  head $ drop steps $  Graph.reachable (Graph.transposeG g) startNode-}

--getPlayer::Prism' Character Player -> Player
--getPlayer charPrism =  traverseOf each charPrism allPlayers


chapel:: Tile
chapel = defaultTile 1

hills1 :: Tile
hills1 = defaultTile 2

sentinel::Tile
sentinel = defaultTile 3

woods1::Tile
woods1 = defaultTile 4

graveyard::Tile
graveyard = defaultTile 5

fields1::Tile
fields1 = defaultTile 6

village::Tile
village = defaultTile 7

fields2::Tile
fields2= defaultTile 8

forest::Tile
forest = defaultTile 9

plains1::Tile
plains1 = defaultTile 10

ruins::Tile
ruins = defaultTile 11

fields3::Tile
fields3 = defaultTile 12

tavern::Tile
tavern = defaultTile 13

plains2::Tile
plains2 = defaultTile 14

woods2::Tile
woods2 = defaultTile 15

plains3::Tile
plains3 = defaultTile 16

hills2::Tile
hills2 = defaultTile 17

fields4::Tile
fields4 = defaultTile 18

city::Tile
city = defaultTile 19

fields5::Tile
fields5 = defaultTile 20

woods3::Tile
woods3 = defaultTile 21

plains4::Tile
plains4 = defaultTile 22

crags::Tile
crags = defaultTile 23

fields6::Tile
fields6 = defaultTile 24



spaces::[Space]
spaces = [ChapelSpace chapel
           , HillsSpace hills1
           , SentinelSpace sentinel
           , WoodsSpace woods1
           , GraveyardSpace graveyard
           , FieldsSpace fields1
           , VillageSpace village
           , FieldsSpace fields2
           , ForestSpace forest
           , PlainsSpace plains1
           , RuinsSpace ruins
           , FieldsSpace fields3
           , TavernSpace tavern
           , PlainsSpace plains2
           , WoodsSpace woods2
           , PlainsSpace plains3
           , HillsSpace hills2
           , FieldsSpace fields4
           , CitySpace city
           , FieldsSpace  fields5
           , WoodsSpace woods3
           , PlainsSpace plains4
           , CragsSpace crags
           , FieldsSpace fields6]




