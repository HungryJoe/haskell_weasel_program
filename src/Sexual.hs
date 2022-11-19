module Sexual where
import System.Random (RandomGen)

spawnNextGeneration :: RandomGen g => String -> Float -> Int -> ([String], g) -> ([String], g)
spawnNextGeneration target mutChance numFittestToPick (current, randomGen) = (current, randomGen)
-- Pick the numFittestToPick from current
-- Recombine them pairwise
-- Mutate the offspring with mutChance

spawnInitialGeneration :: RandomGen g => g -> Int -> ([String], g)
spawnInitialGeneration randomGen orgLen = ([], randomGen)

evolve :: RandomGen g => String -> Float -> Int -> ([String], g) -> ([[String]], g)
evolve target mutChance numFittestToPick (current, randomGen) = ([], randomGen)
