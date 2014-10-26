module Board where
import Tile
import DList

data Space = FieldsSpace Fields | ForestSpace Forest | RuinsSpace Ruins

type Board = DList Space

createBoard::Board
createBoard = mkDList [FieldsSpace fields, ForestSpace forest, RuinsSpace ruins]

