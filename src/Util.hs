module Util (mutate, calculateFitness, genLetter) where
import System.Random (RandomGen, uniformR)
import Data.Bifunctor (Bifunctor(first))

mutate :: RandomGen g => Float -> (String, g) -> (String, g)
-- Generate a string identical to organism except each letter has a probability * 100% chance to be changed to a random other letter
mutate probability (a:rest, randomGen) = first (a':) $ mutate probability (rest, randomGen')
    where (a', randomGen') = mutateChar probability (a, randomGen)
mutate _ ([], randomGen) = ("", randomGen)

mutateChar :: RandomGen g => Float -> (Char, g) -> (Char, g)
mutateChar probability (ch, randomGen)
    | doMutate = genLetter randomGen 
    | otherwise = (ch, randomGen')
    where (doMutate, randomGen') = (fst (roll randomGen) < probability, snd (roll randomGen))
          roll = uniformR (0 :: Float, 1 :: Float)

-- Fitness is in [0, min (length left) (length right)]
calculateFitness :: String -> String -> Int
calculateFitness left right = sum $ zipWith score left right
    where score x y = if x == y then 1 else 0

genLetter :: RandomGen g => g -> (Char, g)
genLetter randomGen = first intToChar (uniformR (0, 26) randomGen)
    where intToChar int
              | int < 26 = ['a'..'z'] !! int
              | otherwise = ' '
