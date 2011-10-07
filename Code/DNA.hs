module DNA
where

import Data.Tree
import Data.Foldable

data NukeTide = A | T | C | G deriving (Read, Show, Eq, Ord, Enum)

type Motif = [NukeTide] -- Not all [NukeTide] are motifs, could be confusing
type DNA = [[NukeTide]]

readLine :: String -> [NukeTide]
readLine xs = [ read [s] | s <- xs ] 

readDNA :: [String] -> DNA
readDNA = map readLine


newtype SimpleMotifTree a = MTree (Tree a) deriving (Show, Eq)
-- When folding over a simple motif tree only the leaf nodes
-- are included.

instance Foldable MotifTree where
	foldr _ b (MTree (Node _ [])) = b
	foldr f b (MTree (Node x xs)) = foldr (\x y -> foldr f y x) b xs
