module MotifTrees
where

import DNA
import Data.Tree
import Data.Foldable
import Prelude hiding (foldr)


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

newtype SimpleMotifTree a = MTree (Tree a) deriving (Show, Eq)
-- When folding over a simple motif tree only the leaf nodes
-- are included.

instance Foldable SimpleMotifTree where
	foldr _ b (MTree (Node _ [])) = b
	foldr f b (MTree (Node x xs)) = foldr (\x y -> foldr f y x) b xs

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
