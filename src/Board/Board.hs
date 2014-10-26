module Board.Board where
import Board.Tile
import DList

data Space = FieldsSpace Fields
  | ForestSpace Forest
  | RuinsSpace Ruins
  | TavernSpace Tavern
  | PlainsSpace Plains
  | WoodsSpace Woods
  | HillsSpace Hills
  | CitySpace City
  | ChapelSpace Chapel
  | SentinelSpace Sentinel
  | GraveyardSpace Graveyard
  | VillageSpace Village
  | CragsSpace Crags

type Board = DList Space

createBoard::Board
createBoard = mkDList [ChapelSpace chapel, HillsSpace hills, SentinelSpace sentinel, WoodsSpace woods, GraveyardSpace graveyard,
  FieldsSpace fields, VillageSpace village, FieldsSpace fields, ForestSpace forest, PlainsSpace plains, RuinsSpace ruins, FieldsSpace fields,
  TavernSpace tavern, PlainsSpace plains, WoodsSpace woods, PlainsSpace plains, HillsSpace hills, FieldsSpace fields, CitySpace city,
  FieldsSpace fields, WoodsSpace woods, PlainsSpace plains, CragsSpace crags, FieldsSpace fields]

