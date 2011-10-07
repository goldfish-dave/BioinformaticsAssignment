module MotifTrees
where

import DNA
import Data.Tree
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr)

newtype SimpleMotifTree a = SMTree (Tree a) deriving (Show, Eq)

-- When folding over a simple motif tree only the leaf nodes
-- are included.
instance Foldable SimpleMotifTree where
	foldr f b (SMTree (Node x [])) = f x b
	foldr f b (SMTree (Node _ xs)) = foldr (\j k -> foldr f k (SMTree j)) b xs

searchTree :: Int -> Tree Motif
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

testTree = Node 5 [
			Node 3 [
			 Node 1 [],
			 Node 6 []],
			Node 9 [
			 Node 8 [],
			 Node 10 [Node 0 []]]]
