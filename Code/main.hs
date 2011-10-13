module Main
where

import System.Environment(getArgs)
import Data.Char(toLower)

import MedianSearch
import DNA
import Distances
import MotifTrees
import Prelude hiding (foldr)

import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM

--file = "Data/mine-test.txt"
file = "Data/text-book-8-mer.txt"
params = (error "You must provide a file (-o <filename>)",
		  stmMedianSearch, --default
		  1, -- default
		  error "You must provide a motif length (-l <motifLength>)",
		  False)

main :: IO ()
main = getArgs >>= parseArgs params
--main = getArgs >>= print

parseArgs :: (IO DNA,Int -> DNA -> Int -> IO (Motif,[Position]),Int,Int,Bool) -> [String] -> IO ()
parseArgs (file,algorithm,forks,motifLength,display) [] = do
	dna <- file
	let l = motifLength
	let f = forks
	case display of
		True -> algorithm f dna l >>= \(m,pos) -> (print $ "Motif: " ++ show m) >> mapM_ print (highlightPositions dna pos motifLength)
		False -> algorithm f dna l >>= print
parseArgs (file, algorithm, forks, motifLength,display) (x:y) = case x of
	"-h" -> putStr usage
	"-f" -> parseArgs (file,algorithm, getForks (head y), motifLength,display) $ tail y
	"-a" -> parseArgs (file,selectAlgorithm (head y), forks, motifLength,display) $ tail y
	"-l" -> parseArgs (file,algorithm,forks,getMotifLength (head y),display) $ tail y
	"-o" -> parseArgs (getFile (head y),algorithm,forks,motifLength,display) $ tail y
	"-d" -> parseArgs (file,algorithm,forks,motifLength,True) y
	_ -> putStr usage

usage :: String
usage = "Usage: \t -f <forks> \n\t\t An optional argument to specify the number of forks to use. \n\t -a <algorithm \n\t\t An optional argument to specify the algorithm (options are simple, bounding, locking and stm<default>)\n\t -l \t <motifLength> \n\t\t A non optional argument to specify the length of the motif to be searched for. \n\t -o \t <fileLocation> \n\t\t A non optional argument to specify the location of the file to be read. \n\t -d \t No arguments \n\t\t An optional argument that enables a human \"friendly\" output format.\n"

getFile :: String -> IO DNA
getFile file = readFile file >>= return . readDNA . lines

getForks :: String -> Int
getForks forks = readInt forks

getMotifLength :: String -> Int
getMotifLength l = readInt l

selectAlgorithm :: String -> (Int -> DNA -> Int -> IO (Motif,[Position])) 
selectAlgorithm algorithm = case map toLower algorithm of
	"simple" -> \_ dna l -> return $ simpleMedianSearch dna l      -- wrapped in a lambda to ignore forks
	"bounding" -> \_ dna l -> return $ boundingMedianSearch dna l
	"locking" -> lockingMedianSearch
	"stm" -> stmMedianSearch
	_ -> error $ show algorithm ++ " is not an available algorithm."

readInt :: String -> Int
readInt "inf" = maxBound :: Int
readInt s = case reads s of
	[(x,"")] -> if x > 1 then x else error "You must input a positive number."
	_ -> error $ "Cannot coerce an int: " ++ show s
