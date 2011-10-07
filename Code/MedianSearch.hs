module MedianSearch
where

import Distances
import DNA

import Data.Tree

infinity = 1000 :: Int

type BestWord = (Motif, Int)  -- (BestWord, BestDistance)
		    				  -- Used to represent a motif and its score
-- newtype BestWord = BestWord (Motif, Int)
-- It would be nice to have BestWord as a newtype if I could
-- think of a clean way to define an Ordering on them

type BranchShoot = (Int, Motif) -- (Motif length, Motif)
                                -- Used to represent a (possibly incomplete) motif
								-- in a search tree.

-- NB: BestWord and BranchShoot hold the same datatypes
-- but are made distinct because their purposes are different.

type MotifTree = Tree Motif

showMotifTree :: MotifTree -> String
showMotifTree = drawTree . fmap show

searchTree :: Int -> MotifTree
-- This function takes a motif length l and returns a
-- search tree containing all possible motifs of length l
-- as its leaf nodes.
searchTree l
	| l < 0 = error "searchTree: must use positive l!"
	| otherwise = unfoldTree (branchTree) (seed)
	where
		branchTree :: BranchShoot -> (Motif, [BranchShoot])
		branchTree (0, motif) = ( motif, [] )
		branchTree (n, motif) = 
			let
				newMotifs = map (\base -> motif ++ [base]) [A,T,C,G]
				nextLevel = repeat (n-1)
				branchShoots = nextLevel `zip` newMotifs
			in	(motif, branchShoots)
		seed = (l, []) -- [] represents a motif of length 0

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
