module Ace where
import System.Random
import Data.Bifunctor (first)
import Data.List (maximumBy)
import Util

data AceWeasel = AceWeasel{popSize :: Int, mutChance :: Float}

instance EvolutionaryAlgorithm AceWeasel where
    spawnNextGeneration target AceWeasel {popSize=_popSize, mutChance=_mutChance} current = first (replicate _popSize . head . findMostFit target 1) (mutateAll _mutChance current)
    spawnInitialGeneration length AceWeasel {popSize=_popSize, mutChance=_mutChance} randomGen = (replicate _popSize initialOrg, randomGen')
        where (initialOrg, randomGen') = spawnOrganism randomGen length
