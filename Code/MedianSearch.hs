module MedianSearch
where

import Distances
import DNA
import MotifTrees

infinity = 1000 :: Int


bfMedSerch :: DNA -> Int -> [NukeTide]
bfMedSerch dna l = fst $ ms tree ([],infinity)
	where
		tree = searchTree l

		ms :: Tree [NukeTide] -> ([NukeTide],Int) -> ([NukeTide],Int)
		ms (Node x []) (bw,bd)
			| bd' < bd = (x,bd')
			| otherwise = (bw,bd)
			where bd' = tDist x
		ms (Node _ xs) bwd = foldr (\a b -> testWord (ms a b) b) bwd xs

		testWord (w,d) (w',d')
			| d < d' = (w,d)
			| otherwise = (w',d')
		tDist = totDist dna l

		searchTree :: Int -> Tree [NukeTide]
		searchTree n 
			| n < 0 = error "searchTree: must use positive n!"
			| otherwise = unfoldTree seed ([],n)
			where
		seed (nk, 0) = (nk, [])
		seed (nk, i) = (nk, map (\x -> nk ++ [x]) [A,T,C,G] `zip` [i-1,i-1..])

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
