module DNA
where

data NukeTide = A | T | C | G deriving (Read, Show, Eq, Ord, Enum)

type DNA = [[NukeTide]]
