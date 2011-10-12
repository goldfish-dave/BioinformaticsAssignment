module Distances
where

import DNA
import Data.List(tails)
import Data.Foldable(minimumBy)


scoreFunction :: DNA -> Int -> (Motif -> ScorePositions)
scoreFunction dna l = distanceFunction
	where
		allMotifPos = map (getMotifPos l) dna :: [[(Motif, Position)]]

		distanceFunction motif = sumScores . minimumScores $ allMotifPos
			where
				sumScores = foldr collectScorePos (0,[])
				minimumScores = map (minimumBy compareScorePos . calculateScores)
				calculateScores = map (getScorePos motif)

hammingDistance :: Motif -> Motif -> Int
hammingDistance xs ys = go 0 xs ys
	where
		go !acc [] [] = acc
		go !acc xs [] = acc
		go !acc [] ys = acc
		go !acc (x:xs) (y:ys) = case (x == y) of
			True -> go acc xs ys
			False -> go (acc+1) xs ys

getScorePos :: Motif -> MotifPos -> (Int, Position)
getScorePos motif (motif',pos) = (hammingDistance motif motif', pos)

collectScorePos :: (Int,Position) -> ScorePositions -> ScorePositions
collectScorePos (score, pos) (totalScore, positions) = (score + totalScore, positions ++ [pos])

getMotifPos :: Int -> [Nucleotide] -> [(Motif,Position)]
getMotifPos n nukeTides = (map (take n) $ take count $ tails nukeTides) `zip` [1..]
	where count = length nukeTides - n + 1

