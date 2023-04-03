module Allo (spawnInitialGeneration, evolve, spawnNextGeneration) where
import System.Random (RandomGen, Random (random))
import Util (spawnOrganism, findMostFit, mutate, calculateFitness, mutateAll)
import Data.Set (cartesianProduct, fromList, toList, filter)
import Data.Bifunctor (Bifunctor(first))

spawnNextGeneration :: RandomGen g => String -> Float -> Int -> ([String], g) -> ([String], g)
spawnNextGeneration target mutChance numFittestToPick (current, randomGen) = (mutatedOffspring, randomGen'')
    where parents = fromList $ findMostFit target numFittestToPick current
          parentPairs = toList $ Data.Set.filter (uncurry (/=)) $ cartesianProduct parents parents
          (offspring, randomGen') = recombinePairs (parentPairs, randomGen)
          (mutatedOffspring, randomGen'') = mutateAll mutChance (offspring, randomGen')

recombinePairs :: RandomGen g => ([(String, String)], g) -> ([String], g)
recombinePairs ([], randomGen) = ([], randomGen)
recombinePairs (parents:rest, randomGen) = first (offspring:) $ recombinePairs (rest, randomGen')
    where (offspring, randomGen') = recombinePair parents randomGen

recombinePair :: RandomGen g => (String, String) -> g -> (String, g)
recombinePair (parentA, parentB) = go zippedParents
    where zippedParents = zip parentA parentB
          go [] randomGen = ([], randomGen)
          go ((a,b):rest) randomGen = first (result:) $ go rest randomGen'
            where (coinFlip, randomGen') = random randomGen
                  result = if coinFlip then a else b

spawnInitialGeneration :: RandomGen g => g -> Int -> Int -> ([String], g)
spawnInitialGeneration randomGen _ 0 = ([], randomGen)
spawnInitialGeneration randomGen orgLen numOrgs = (nextOrg : restOfList, randomGen'')
    where (nextOrg, randomGen') = spawnOrganism randomGen orgLen
          (restOfList, randomGen'') = spawnInitialGeneration randomGen' orgLen (numOrgs - 1)

evolve :: RandomGen g => String -> Float -> Int -> ([String], g) -> ([[String]], g)
evolve target mutChance numFittestToPick current@(currentGen, randomGen)
    | calculateFitness target (head $ findMostFit target 1 currentGen) == length target = ([currentGen], randomGen)
    | otherwise = first (nextGen:) $ evolve target mutChance numFittestToPick next
    where next@(nextGen, _) = spawnNextGeneration target mutChance numFittestToPick current
