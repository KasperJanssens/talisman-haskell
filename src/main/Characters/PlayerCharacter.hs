{-# LANGUAGE TemplateHaskell #-}
module Characters.PlayerCharacter where
import Object
import Characters.Follower
import Characters.Alignment
import Control.Lens
import Data.Maybe
import Board.Board


data Player = Player {
  __strength::Int,
  __craft::Int,
  _fate::Int,
  _gold::Int,
  __life::Int,
  _objects::[Object],
  _followers::[Follower],
  _alignment::Alignment,
  _place::Int
} deriving (Eq, Show, Ord)

makeLenses ''Player

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

data Character =  OgreChieftain Player SelectTileFunc
              | Thief Player SelectTileFunc
              | Wizard Player SelectTileFunc

instance Show Character where
  show (OgreChieftain player _) = show player
  show (Wizard player _) = show player
  show (Thief player _) = show player

instance Eq Character where
  (==) (OgreChieftain player1 _) (OgreChieftain player2 _) = player1 == player2
  (==) (Wizard player1 _) (Wizard player2 _ ) = player1 == player2
  (==) (Thief player1 _) (Thief player2 _) = player1 == player2
  (==)  _  _ = False


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
  _place=5
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
  _place=23
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
  _place=19
}

getPlayer::ReifiedPrism' Character Player -> [Character] -> Player
getPlayer characterPrism chars = head $ mapMaybe (preview $ runPrism characterPrism) chars

getCharacter::ReifiedPrism' Character Player -> [Character] -> Character
getCharacter prism chars = review (runPrism prism) $ getPlayer prism chars

getPrism::Character -> ReifiedPrism' Character (Player, SelectTileFunc)
getPrism (Thief _ _ ) = Prism _Thief
getPrism (Wizard _ _ ) = Prism _Wizard
getPrism (OgreChieftain _ _) = Prism _OgreChieftain

allPlayers::[SelectTileFunc -> Character]
allPlayers=[OgreChieftain ogreChieftain
          , Wizard wizard
          , Thief thief
          ]