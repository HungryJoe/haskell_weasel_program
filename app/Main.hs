module Main where

import Ace (AceWeasel (..))
import qualified Util (spawnInitialGeneration, evolve)
import qualified Allo (evolve, spawnInitialGeneration)
import System.Environment (getArgs)
import System.Random (initStdGen)

-- main <target> [mut-chance] [max-iterations]
main :: IO ()
main = do
  args <- getArgs
  let target = head args
  let mutChance = if length args == 3 then read (args !! 1) else 0.05
  let numCopies = if length args == 3 then read (args !! 2) else 100
  rng <- initStdGen
  let aceWeasel = AceWeasel {popSize=numCopies, mutChance=mutChance}
  let aceInitialGen = Util.spawnInitialGeneration (length target) aceWeasel rng
  let (aceGenerations, rng') = Util.evolve target aceWeasel aceInitialGen
  let alloInitialGen = Allo.spawnInitialGeneration rng (length target) numCopies
  let (alloGenerations, rng') = Allo.evolve target mutChance numCopies alloInitialGen
  print $ "It took " ++ show (length aceGenerations) ++ " to asexually generate:"
  print $ head $ last aceGenerations
  print $ "It took " ++ show (length alloGenerations) ++ " to allosexually generate:"
  print $ last alloGenerations
