module Allo where
import System.Random (RandomGen, Random (random))
import Util
import Data.Set (cartesianProduct, fromList, toList, filter)
import Data.Bifunctor (Bifunctor(first))

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

data AlloWeasel = AlloWeasel {popSize :: Int, mutChance :: Float}
instance EvolutionaryAlgorithm AlloWeasel where
    spawnInitialGeneration orgLen AlloWeasel {popSize=_popSize, mutChance=_mutChance} randomGen = go randomGen _popSize
        where go randomGen 0 = ([], randomGen)
              go randomGen numOrgs = first (nextOrg :) $ go randomGen' (numOrgs - 1)
                where (nextOrg, randomGen') = spawnOrganism randomGen orgLen
    spawnNextGeneration target AlloWeasel {popSize=_popSize, mutChance=_mutChance} (current, randomGen) = (findMostFit target _popSize mutatedOffspring, randomGen'')
        where parents = fromList current
              parentPairs = toList $ Data.Set.filter (uncurry (/=)) $ cartesianProduct parents parents
              (offspring, randomGen') = recombinePairs (parentPairs, randomGen)
              (mutatedOffspring, randomGen'') = mutateAll _mutChance (offspring, randomGen')
