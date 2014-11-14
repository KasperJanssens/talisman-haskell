{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Board.Board where
import Control.Lens
import Characters.Follower
import Adventure
import Object
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

boardLayout::Map Int (((Tile -> Tile) -> Space -> Space, Space -> Maybe Tile), Neighbours)
boardLayout = fromList $ zip [1..24]
           [((over _ChapelSpace, preview _ChapelSpace),[2,24])
           , ((over _Hills1Space, preview _Hills1Space),[1,3])
           , ((over _SentinelSpace, preview _SentinelSpace),[2,4])
           , ((over _Woods1Space, preview _Woods1Space),[3,5])
           , ((over _GraveyardSpace, preview _GraveyardSpace),[4,6])
           , ((over _Fields1Space, preview _Fields1Space),[5,7])
           , ((over _VillageSpace, preview _VillageSpace),[6,8])
           , ((over _Fields2Space, preview _Fields2Space),[7,9])
           , ((over _ForestSpace, preview _ForestSpace),[8,10])
           , ((over _Plains1Space, preview _Plains1Space),[9,11])
           , ((over _RuinsSpace, preview _RuinsSpace),[10,12])
           , ((over _Fields3Space, preview _Fields3Space),[11,13])
           , ((over _TavernSpace, preview _TavernSpace),[12,14])
           , ((over _Plains2Space, preview _Plains2Space),[13,15])
           , ((over _Woods2Space, preview _Woods2Space),[14,16])
           , ((over _Plains3Space, preview _Plains3Space),[15,17])
           , ((over _Hills2Space, preview _Hills2Space),[16,18])
           , ((over _Fields4Space, preview _Fields4Space),[17,19])
           , ((over _CitySpace, preview _CitySpace),[18,20])
           , ((over _Fields5Space, preview _Fields5Space),[19,21])
           , ((over _Woods3Space, preview _Woods3Space),[20,22])
           , ((over _Plains4Space, preview _Plains4Space),[21,23])
           , ((over _CragsSpace, preview _CragsSpace),[22,24])
           , ((over _Fields6Space, preview _Fields6Space),[23,1])
           ]

getMovingOptions::Int -> Int -> [((Tile -> Tile) -> Space -> Space, Space -> Maybe Tile)]
getMovingOptions dieRoll = calculatePossibleMoves dieRoll (-1)


calculatePossibleMoves::Int -> Int -> Int -> [((Tile -> Tile) -> Space -> Space, Space -> Maybe Tile)]
calculatePossibleMoves stepsLeft former current
  | stepsLeft == 0 = [ fst $ findWithDefault undefined current boardLayout]
  | otherwise = List.concatMap (calculatePossibleMoves (stepsLeft - 1) current) directNeighbours
     where directNeighbours = List.filter (/= former) $ snd $ findWithDefault undefined current boardLayout



