module Battle where
import Characters.PlayerCharacter
import System.Random
import Control.Monad.State

main::IO()
main = do
  g <- getStdGen
  let _ = randomRs (1, 6) g :: [Int]
  return ()

rollDie :: State StdGen Int
rollDie = do
    generator <- get
    let (roll, newGenerator) = random generator
    put newGenerator
    return roll

fight::(Living a) => (a -> Int) ->  (a, a) -> State StdGen (a,a)
fight attribute (attacker,defender) = do
   attackerRoll <- rollDie
   defenderRoll <- rollDie
   let attackerValue = attackerRoll + attribute attacker
       defenderValue = defenderRoll + attribute defender
   case  compare attackerValue defenderValue of
       LT -> return (loseLife attacker, defender)
       GT -> return (attacker, loseLife defender)
       EQ -> return (attacker,defender)

fightStrength:: (HasStrength a, Living a) => (a,a) -> State StdGen (a,a)
fightStrength = fight strength

fightCraft:: (HasCraft a, Living a) =>  (a,a) -> State StdGen (a,a)
fightCraft = fight craft
