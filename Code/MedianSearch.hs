module MedianSearch
where

import Distances
import DNA
import MotifTrees
import Data.Tree
import Data.Foldable (minimumBy)

infinity = 1000 :: Int

simpMedSearch :: DNA -> Int -> Motif
-- Takes an n x t array of nucleotides and a length l
-- and outputs the most likely motif of length l
-- NB: This function does not implement branch and bounding
simpMedSearch dna l = fst . minimumBy (\a b -> compare (snd a) (snd b)) $ map bestWord motifs
	where
		totalDistance = scoreFunction dna l

		bestWord word = (word, totalDistance word)

		motifs = simpleTraverse $ searchTree l

bnbMedSearch :: DNA -> Int -> Motif
bnbMedSearch dna l = fst $ bnbTraverse totalDistance (searchTree l) ([],infinity)
	where
		totalDistance = scoreFunction dna l


bnbTraverse :: (Motif -> Int) -> Tree Motif -> BestWord -> BestWord
bnbTraverse totalDistance (Node x []) (motif, score)
	| score' < score = (x    , score')
	| otherwise      = (motif, score )
	where score'  = totalDistance x
bnbTraverse totalDistance (Node x xs) (motif, score)
	| score' < score = foldr (bnbTraverse totalDistance) (motif, score) xs
	| otherwise      = (motif, score)
	where score' = totalDistance x

{-

medianSearch :: DNA -> Int -> Motif
medianSearch = 
bnbMedianSearch = branchAndBound searchTree totalDistance
	where
		totalDistance :: Motif -> Int

branchAndBound :: Tree a -> (a -> Int) -> a
-}
