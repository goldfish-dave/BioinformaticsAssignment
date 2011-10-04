module DNA
where

data NukeTide = A | T | C | G deriving (Read, Show, Eq, Ord, Enum)

type DNA = [[NukeTide]]

readLine :: String -> [NukeTide]
readLine xs = [ read [s] | s <- xs ] 

readDNA :: [String] -> DNA
readDNA = map readLine
