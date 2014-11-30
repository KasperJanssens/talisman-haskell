{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Board.Board where
import qualified Control.Lens as Lens
import Characters.Follower
import Adventure
import Object
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List


data Tile = Tile {
  _tileNumber::Int,
  _freeFollowers::[Follower],
  _freeObjects::[Object],
  _adventures::[Adventure]
} deriving (Eq, Ord, Show)

Lens.makeLenses ''Tile

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

Lens.makePrisms ''Space


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

spaces::[Space]
spaces=[ChapelSpace chapel
       , Hills1Space hills1
       , SentinelSpace sentinel
       , Woods1Space woods1
       , GraveyardSpace graveyard
       , Fields1Space fields1
       , VillageSpace village
       , Fields2Space fields2
       , ForestSpace forest
       , Plains1Space plains1
       , RuinsSpace ruins
       , Fields3Space fields3
       , TavernSpace tavern
       , Plains2Space plains2
       , Woods2Space woods2
       , Plains3Space plains3
       , Hills2Space hills2
       , Fields4Space fields4
       , CitySpace city
       , Fields5Space fields5
       , Woods3Space woods3
       , Plains4Space plains4
       , CragsSpace crags
       , Fields6Space fields6
       ]

boardLayout::Map.Map Int (Lens.ReifiedPrism' Space Tile, Neighbours)
boardLayout = Map.fromList $ zip [1..24]
           [(Lens.Prism _ChapelSpace,[2,24])
           , (Lens.Prism _Hills1Space,[1,3])
           , (Lens.Prism _SentinelSpace,[2,4])
           , (Lens.Prism _Woods1Space,[3,5])
           , (Lens.Prism _GraveyardSpace,[4,6])
           , (Lens.Prism _Fields1Space,[5,7])
           , (Lens.Prism _VillageSpace,[6,8])
           , (Lens.Prism _Fields2Space,[7,9])
           , (Lens.Prism _ForestSpace,[8,10])
           , (Lens.Prism _Plains1Space,[9,11])
           , (Lens.Prism _RuinsSpace,[10,12])
           , (Lens.Prism _Fields3Space,[11,13])
           , (Lens.Prism _TavernSpace,[12,14])
           , (Lens.Prism _Plains2Space,[13,15])
           , (Lens.Prism _Woods2Space,[14,16])
           , (Lens.Prism _Plains3Space,[15,17])
           , (Lens.Prism _Hills2Space,[16,18])
           , (Lens.Prism _Fields4Space,[17,19])
           , (Lens.Prism _CitySpace,[18,20])
           , (Lens.Prism _Fields5Space,[19,21])
           , (Lens.Prism _Woods3Space,[20,22])
           , (Lens.Prism _Plains4Space,[21,23])
           , (Lens.Prism _CragsSpace,[22,24])
           , (Lens.Prism _Fields6Space,[23,1])
           ]


getMovingOptions::Int -> Int -> [Tile]
getMovingOptions dieRoll currentSpace =
     List.foldl (\tiles prism -> let matchingTiles= Maybe.mapMaybe (Lens.preview . Lens.runPrism $ prism) spaces
                                 in tiles ++ matchingTiles) [] spacePrisms
     where spacePrisms = calculatePossibleMoves dieRoll (-1) currentSpace

calculatePossibleMoves::Int -> Int -> Int -> [Lens.ReifiedPrism' Space Tile]
calculatePossibleMoves stepsLeft former current
  | stepsLeft == 0 = [ fst $ Map.findWithDefault undefined current boardLayout]
  | otherwise = List.concatMap (calculatePossibleMoves (stepsLeft - 1) current) directNeighbours
     where directNeighbours = List.filter (/= former) $ snd $ Map.findWithDefault undefined current boardLayout
