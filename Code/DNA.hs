module DNA
where

import Data.Tree

data Nucleotide = A | T | C | G deriving (Read, Show, Eq)

type Motif = [Nucleotide] 
type DNA = [[Nucleotide]]
type Position = Int
type MotifPos = (Motif,Position)
type ScorePositions = (Int  ,[Position])
type BestWord = (Motif, Int)  -- (BestWord, BestDistance)
		    				  -- Used to represent a motif and its score
-- newtype BestWord = BestWord (Motif, Int)
-- It would be nice to have BestWord as a newtype if I could
-- think of a clean way to define an Ordering on them

type BranchShoot = (Int, Motif) -- (Motif length, Motif)
                                -- Used to represent a (possibly incomplete) motif
								-- in a search tree.

compareScorePos :: (Int, Position) -> (Int, Position) -> Ordering
compareScorePos (score,_) (score',_) = compare score score'

-- NB: BestWord and BranchShoot hold the same datatypes
-- but are made distinct because their purposes are different.

readLine :: String -> [Nucleotide]
readLine xs = [ read [s] | s <- xs ] 

readDNA :: [String] -> DNA
readDNA = map readLine
--
-----------------------------------------------------------------------
-- Tree generation

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
