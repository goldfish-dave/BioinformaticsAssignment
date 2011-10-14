module DNA
where

import Data.Char (toLower, toUpper)
import Data.Tree

data Nucleotide = A | T | C | G deriving (Show, Eq)

readNuc "A" = A
readNuc "T" = T
readNuc "C" = C
readNuc "G" = G
readNuc s = error $ show s ++ " is not a valid nucleotide"

type Motif = [Nucleotide] 
type DNA = [[Nucleotide]]
type Position = Int
type MotifPos = (Motif,Position)
type ScorePositions = (Int  ,[Position])

type BranchShoot = (Int, Motif)

compareScorePos :: (Int, Position) -> (Int, Position) -> Ordering
compareScorePos (score,_) (score',_) = compare score score'

readLine :: String -> [Nucleotide]
readLine xs = [ readNuc [s] | s <- xs ] 

readDNA :: [String] -> DNA
readDNA = map readLine

showMotif :: [Nucleotide] -> String
showMotif = concatMap show

highlightPositions :: DNA -> [Position] -> Int -> [String]
highlightPositions dna positions length = map (highlightPositions' length) $ zip dna positions

highlightPositions' :: Int -> ([Nucleotide],Position) -> String
highlightPositions' length (nukes, pos) = map toLower beforeMotif ++ map toUpper motif ++ map toLower afterMotif
	where
		nukes' = concatMap show nukes :: String
		(beforeMotif, rest) = splitAt pos nukes'
		(motif, afterMotif) = splitAt length rest

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
