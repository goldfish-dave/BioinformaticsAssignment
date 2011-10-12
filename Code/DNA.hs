module DNA
where

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
