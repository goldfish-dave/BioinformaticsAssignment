module DNA
where

data NukeTide = A | T | C | G deriving (Read, Show, Eq, Ord, Enum)

type Motif = [NukeTide] -- Not all [NukeTide] are motifs, could be confusing
type DNA = [[NukeTide]]

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

readLine :: String -> [NukeTide]
readLine xs = [ read [s] | s <- xs ] 

readDNA :: [String] -> DNA
readDNA = map readLine
