module MedianSearch
where

import Distances
import DNA
import MotifTrees
import Data.Foldable

infinity = 1000 :: Int

simpMedSerch :: DNA -> Int -> Motif
-- Takes an n x t array of nucleotides and a length l
-- and outputs the most likely motif of length l
-- NB: This function does not implement branch and bounding
simpMedSerch dna l = minimumBy (bestOf . bestWord) simpleTree
	where
		totalDistance = scoreFunction dna l

		bestWord word = (word, totalDistance word)

		simpleTree = SMTree $ searchTree l

{-
simpMedSerch :: DNA -> Int -> Motif
-- Takes an n x t array of nucleotides and a length l
-- and outputs the most likely motif of length l
-- NB: This function does not implement branch and bounding
simpMedSerch = foldr (bestOf . totalScore) ([], infinity) . searchTree
	where
		ms :: Tree [NukeTide] -> BestWord -> BestWord
		ms (Node x []) b@(bw,bd)       -- Leaf node
			| True = undefined         -- x is better than b
			| otherwise = undefined    -- b ix better than x
		ms (Node _ xs) b@(bw,bd) = foldr (\node currBest -> bestOf (ms node currBest) currBest) b xs
		-- The above line performs depth-first ms over all the children nodes,
		-- returning the best BestWord
{-
- a median search function needs a totalDistance function
-	it returns a motif
- a totalDistance function needs a DNA and takes a motif
-	it returns a distance

medianSearch :: DNA -> Int -> Motif
medianSearch = 
bnbMedianSearch = branchAndBound searchTree totalDistance
	where
		totalDistance :: Motif -> Int

branchAndBound :: Tree a -> (a -> Int) -> a
-}













-}
