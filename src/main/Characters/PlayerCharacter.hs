{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Characters.PlayerCharacter where
import Object
import Characters.Follower
import Characters.Alignment
import Control.Lens
import Data.Maybe
import Board.Board
import Data.String.Builder as StringBuilder
import Data.String
import Data.Ord


data Player = Player {
  __strength::Int,
  __craft::Int,
  _fate::Int,
  _gold::Int,
  __life::Int,
  _objects::[Object],
  _followers::[Follower],
  _alignment::Alignment,
  _place::Int,
  _selectTileFunc::[Tile] -> IO Tile,
  _selectPlayerFunc::[Player] -> IO Player
}


makeLenses ''Player

instance Ord Player where
  compare player1 player2 = comparing (\player -> player ^. _strength) player1 player2


instance Show Player where
 show player = StringBuilder.build $ do
    fromString . show $ player ^. _strength
    fromString . show $ player ^. _craft
    fromString . show $ player ^. fate
    fromString . show $ player ^. _life
    fromString . show $ player ^. objects
    fromString . show $ player ^. followers
    fromString . show $ player ^. alignment
    fromString . show $ player ^. place

instance Eq Player where
  (==) player1 player2 = (player1 ^. _strength   == player2 ^. _strength) &&
                         (player1 ^. _craft      == player2 ^. _craft) &&
                         (player1 ^. fate       == player2 ^. fate) &&
                         (player1 ^. _life       == player2 ^. _life) &&
                         (player1 ^. objects    == player2 ^. objects) &&
                         (player1 ^. followers  == player2 ^. followers) &&
                         (player1 ^. alignment  == player2 ^. alignment) &&
                         (player1 ^. place      == player2 ^. place)



instance Living Player where
    life = view _life
    loseLife = over _life (subtract 1)
    gainLife = over _life (+ 1)

class Living a where
    life:: a -> Int
    loseLife:: a -> a
    gainLife::a->a

instance HasStrength Player where
    strength = view _strength

instance HasCraft Player where
    craft = view _craft

class HasStrength a where
    strength::a -> Int

class HasCraft a where
    craft::a->Int

type SelectTileFunc = [Tile] -> IO Tile

data Character =  OgreChieftain Player
              | Thief Player
              | Wizard Player deriving (Show, Eq)


makeLenses ''Character

makePrisms ''Character

wizard::Player
wizard = Player {
  __strength=2,
  __craft=5,
  _fate=3,
  _gold=1,
  __life=4,
  _objects=[],
  _followers=[],
  _alignment=Evil,
  _place=5,
  _selectTileFunc = return.head,
  _selectPlayerFunc = return.head
}

ogreChieftain::Player
ogreChieftain = Player {
  __strength=5,
  __craft=2,
  _fate=1,
  _gold=1,
  __life=6,
  _objects=[],
  _followers=[],
  _alignment=Neutral,
  _place=23,
  _selectTileFunc = return.head,
  _selectPlayerFunc = return.head
}

thief::Player
thief = Player {
  __strength=3,
  __craft=3,
  _fate=2,
  _gold=1,
  __life=4,
  _objects=[],
  _followers=[],
  _alignment=Neutral,
  _place=19,
  _selectTileFunc = return.head,
  _selectPlayerFunc = return.head
}

getPlayer::ReifiedPrism' Character Player -> [Character] -> Player
getPlayer characterPrism chars = head $ mapMaybe (preview $ runPrism characterPrism) chars

getCharacter::ReifiedPrism' Character Player -> [Character] -> Character
getCharacter prism chars = review (runPrism prism) $ getPlayer prism chars

getPrism::Character -> ReifiedPrism' Character Player
getPrism (Thief _ ) = Prism _Thief
getPrism (Wizard _) = Prism _Wizard
getPrism (OgreChieftain _) = Prism _OgreChieftain

allPlayers::[Character]
allPlayers=[OgreChieftain ogreChieftain
          , Wizard wizard
          , Thief thief
          ]