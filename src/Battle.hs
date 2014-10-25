module Battle where
import PlayerCharacter
import System.Random

main::IO()
main = do
  g <- getStdGen
  let _ = randomRs (1, 6) g :: [Int]
  return ()

fightStrength:: (HasStrength a, Living a) => StdGen -> (a,a) -> (a,a)
fightStrength gen = fight gen strength

fightCraft:: (HasCraft a, Living a) => StdGen -> (a,a) -> (a,a)
fightCraft gen = fight gen craft

fight::(Living a) =>StdGen -> (a -> Int) ->  (a, a) -> (a,a)
fight gen attribute (attacker, defender)  | randomAttack + attribute attacker > randomDefend + attribute defender = (attacker,loseLife defender)
                                          | randomAttack + attribute attacker == randomDefend + attribute defender = (attacker, defender)
                                          | otherwise = (loseLife attacker, defender)
                                          where randomAttack = fst $ random gen
                                                randomDefend = fst $ random gen
