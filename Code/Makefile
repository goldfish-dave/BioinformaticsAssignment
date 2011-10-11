#!/bin/bash

flags="-O2 -threaded --make main -XBangPatterns"

case $1 in
"")
	ghc $flags
	;;
"-f")
	flags="-fllvm -rtsopts "$flags
	ghc $flags
	;;
"-c")
	echo Cleaning object and interface files...
	rm *.o *.hi
	echo Done.
	exit 1
	;;
"-C")
	echo Cleaning object, interface and main files...
	rm *.o *.hi main
	echo Done.
	exit 1
	;;
"-p")
	flags="-prof -auto "$flags
	ghc $flags
	;;
"-h")
	help="[Usage]: This makefile compiles main. \n\t -p \t Compile with profiling enabled. Run with +RTS -p to get a main.prof created. \n\t -f \t Compile with full arguments (-rtsopts -fllvm) \n\t -c \t Clean object and interface files \n\t -C \t Clean object, interface and main files \n\t -h \t Help"
	echo -e $help
	exit 1
	;;
esac



