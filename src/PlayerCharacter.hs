{-# LANGUAGE TemplateHaskell #-}
module PlayerCharacter where
import Object
import Follower
import Alignment
import Locations
import Control.Lens

data Player = Player {
  __strength::Int,
  __craft::Int,
  _fate::Int,
  _gold::Int,
  __life::Int,
  _objects::[Object],
  _followers::[Follower],
  _alignment::Alignment,
  _startingLocation::Locations
}

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

newtype Wizard = Wizard Player
newtype Thief = Thief Player
newtype OgreChieftain = OgreChieftain Player

createWizard::Wizard
createWizard = Wizard Player {
  __strength=2,
  __craft=5,
  _fate=3,
  _gold=1,
  __life=4,
  _objects=[],
  _followers=[],
  _alignment=Evil,
  _startingLocation=Graveyard
}

createOgreChieftain::OgreChieftain
createOgreChieftain = OgreChieftain Player {
  __strength=5,
  __craft=2,
  _fate=1,
  _gold=1,
  __life=6,
  _objects=[],
  _followers=[],
  _alignment=Neutral,
  _startingLocation=Crags
}

createThief::Thief
createThief = Thief Player {
  __strength=3,
  __craft=3,
  _fate=2,
  _gold=1,
  __life=4,
  _objects=[],
  _followers=[],
  _alignment=Neutral,
  _startingLocation=City
}