{-# LANGUAGE TemplateHaskell #-}
module Board.Board where
import qualified Board.Tile as Tile
import Characters.PlayerCharacter
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Tuple as Tuple

data Space = FieldsSpace Tile.Fields
  | ForestSpace Tile.Forest
  | RuinsSpace Tile.Ruins
  | TavernSpace Tile.Tavern
  | PlainsSpace Tile.Plains
  | WoodsSpace Tile.Woods
  | HillsSpace Tile.Hills
  | CitySpace Tile.City
  | ChapelSpace Tile.Chapel
  | SentinelSpace Tile.Sentinel
  | GraveyardSpace Tile.Graveyard
  | VillageSpace Tile.Village
  | CragsSpace Tile.Crags deriving (Eq, Ord)

makePrisms ''Space

vertexToSpace::Map.Map Graph.Vertex Space
vertexToSpace = Map.fromList  spaces

{-spaceToVertex::Map.Map Space [Graph.Vertex]
spaceToVertex = Map.fromList spaceAndVerticesTuples
                where sortedBySpace =  List.groupBy (\(_,s1) (_,s2) -> s1 == s2) spaces
                      spaceAndVerticesTuples = List.map (Tuple.swap . over _2 head . unzip) sortedBySpace-}


createBoard::Graph.Graph
createBoard = Graph.buildG (firstSpaceNumber, lastSpaceNumber) $ zip [firstSpaceNumber..lastSpaceNumber] $ [firstSpaceNumber+1 .. lastSpaceNumber ] ++[firstSpaceNumber]
              where firstSpaceNumber = fst $ head spaces
                    lastSpaceNumber = fst $ last spaces

goRight::Int -> Graph.Graph -> Int -> Int
goRight steps g startNode =  head $ drop steps $ Graph.reachable g startNode

goLeft::Int -> Graph.Graph -> Int -> Int
goLeft steps g startNode =  head $ drop steps $  Graph.reachable (Graph.transposeG g) startNode



addPlayer::Character -> Space -> Space
addPlayer p =
  over _CragsSpace (\(Tile.Crags crag) -> Tile.Crags $ addPlayerToTile p crag)
   . over _GraveyardSpace (\(Tile.Graveyard graveyard) -> Tile.Graveyard $ addPlayerToTile p graveyard)
   . over _CitySpace (\(Tile.City city) -> Tile.City $ addPlayerToTile p city)

addPlayerToTile::Character -> Tile.Tile -> Tile.Tile
addPlayerToTile p = over Tile.players (\playerList -> p:playerList)

spaces::[(Int,Space)]
spaces = zip [1..] [ChapelSpace Tile.chapel, HillsSpace Tile.hills, SentinelSpace Tile.sentinel, WoodsSpace Tile.woods, GraveyardSpace Tile.graveyard,
  FieldsSpace Tile.fields, VillageSpace Tile.village, FieldsSpace Tile.fields, ForestSpace Tile.forest, PlainsSpace Tile.plains, RuinsSpace Tile.ruins, FieldsSpace Tile.fields,
  TavernSpace Tile.tavern, PlainsSpace Tile.plains, WoodsSpace Tile.woods, PlainsSpace Tile.plains, HillsSpace Tile.hills, FieldsSpace Tile.fields, CitySpace Tile.city,
  FieldsSpace Tile.fields, WoodsSpace Tile.woods, PlainsSpace Tile.plains, CragsSpace Tile.crags, FieldsSpace Tile.fields]



{-createBoard::Board
createBoard = mkDList [ChapelSpace Tile.chapel, HillsSpace Tile.hills, SentinelSpace Tile.sentinel, WoodsSpace Tile.woods, GraveyardSpace Tile.graveyard,
  FieldsSpace Tile.fields, VillageSpace Tile.village, FieldsSpace Tile.fields, ForestSpace Tile.forest, PlainsSpace Tile.plains, RuinsSpace Tile.ruins, FieldsSpace Tile.fields,
  TavernSpace Tile.tavern, PlainsSpace Tile.plains, WoodsSpace Tile.woods, PlainsSpace Tile.plains, HillsSpace Tile.hills, FieldsSpace Tile.fields, CitySpace Tile.city,
  FieldsSpace Tile.fields, WoodsSpace Tile.woods, PlainsSpace Tile.plains, CragsSpace Tile.crags, FieldsSpace Tile.fields]


placePlayers :: Board -> [Character] -> Board
placePlayers = foldl $ flip placePlayer

placePlayer:: Character -> Board -> Board
placePlayer player@(OgreChieftain _) board = apply (addPlayer player) cragsSpace
                                             where cragsSpace = goto board (CragsSpace Tile.crags)
placePlayer player@(Wizard _) board = apply (addPlayer player) graveyardSpace
                                             where graveyardSpace = goto board (GraveyardSpace Tile.graveyard)
placePlayer player@(Thief _) board = apply (addPlayer player) citySpace
                                             where citySpace = goto board (CitySpace Tile.city)-}
