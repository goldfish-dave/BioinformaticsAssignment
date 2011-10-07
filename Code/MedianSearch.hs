module MedianSearch
where

import Distances
import DNA
import MotifTrees
import Data.Foldable
import Prelude hiding (foldr)

infinity = 1000 :: Int

simpMedSearch :: DNA -> Int -> Motif
-- Takes an n x t array of nucleotides and a length l
-- and outputs the most likely motif of length l
-- NB: This function does not implement branch and bounding
simpMedSearch dna l = fst $ foldr' (bestOf . bestWord) ([],infinity) simpleTree
	where
		totalDistance = scoreFunction dna l

		bestWord word = (word, totalDistance word)

		simpleTree = SMTree $ searchTree l

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
