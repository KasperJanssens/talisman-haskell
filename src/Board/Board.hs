{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Board.Board where
import Control.Lens
import Characters.Follower
import Adventure
import Object
import Debug.Trace
import Data.Map
import qualified Data.List as List

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




data Space = Fields1Space Tile
  | Fields2Space Tile
  | Fields3Space Tile
  | Fields4Space Tile
  | Fields5Space Tile
  | Fields6Space Tile
  | ForestSpace Tile
  | RuinsSpace Tile
  | TavernSpace Tile
  | Plains1Space Tile
  | Plains2Space Tile
  | Plains3Space Tile
  | Plains4Space Tile
  | Woods1Space Tile
  | Woods2Space Tile
  | Woods3Space Tile
  | Hills1Space Tile
  | Hills2Space Tile
  | CitySpace Tile
  | ChapelSpace Tile
  | SentinelSpace Tile
  | GraveyardSpace Tile
  | VillageSpace Tile
  | CragsSpace Tile deriving (Eq, Ord, Show)

makePrisms ''Space

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

type Neighbours=[Int]

spaces::Map Int (Space, Neighbours)
spaces = fromList $ zip [1..24] [(ChapelSpace chapel,[2,24])
           , (Hills1Space hills1,[1,3])
           , (SentinelSpace sentinel,[2,4])
           , (Woods1Space woods1, [3,5])
           , (GraveyardSpace graveyard, [4,6])
           , (Fields2Space fields1, [5,7])
           , (VillageSpace village, [6,8])
           , (Fields2Space fields2, [7,9])
           , (ForestSpace forest, [8,10])
           , (Plains1Space plains1, [9,11])
           , (RuinsSpace ruins, [10,12])
           , (Fields3Space fields3,[11,13])
           , (TavernSpace tavern,[12,14])
           , (Plains2Space plains2,[13,15])
           , (Woods2Space woods2,[14,16])
           , (Plains3Space plains3,[15,17])
           , (Hills2Space hills2,[16,18])
           , (Fields4Space fields4,[17,19])
           , (CitySpace city,[18,20])
           , (Fields5Space  fields5,[19,21])
           , (Woods3Space woods3,[20,22])
           , (Plains4Space plains4,[21,23])
           , (CragsSpace crags,[22,24])
           , (Fields6Space fields6,[23,1])
           ]
           
calculatePossibleMoves::Int -> Int -> Int -> [Space]
calculatePossibleMoves dieRoll current former
  | dieRoll == 1 =  List.map (\neighbour -> fst $ findWithDefault 25 neighbour spaces) unvisitedNeighbours
  | otherwise = undefined
     where unvisitedNeighbours = List.filter (/= former) $ snd $ findWithDefault 25 current spaces


{-getPossibleMoves::Int->Int->[Space]
getPossibleMoves dieRoll current 
    | dieRoll == 1 =
       let neighbours = snd $ spaces !! current in
       List.map (fst . (!!) spaces ) neighbours
    | otherwise = calculatePossibleMoves dieRoll current current-}
   
