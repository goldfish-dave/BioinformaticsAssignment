#!/bin/bash

case $1 in
"")
	ghc --make -O2 main
	;;
"-llvm")
	ghc --make -O2 main -fllvm
	;;
"-c")
	echo Cleaning object and interface files...
	rm *.o *.hi
	echo Done.
	;;
"-C")
	echo Cleaning object, interface and main files...
	rm *.o *.hi main
	echo Done.
	;;
"-h")
	help="[Usage]: This makefile compiles main. \n\t -llvm \t Compile with llvm (you will need this installed) \n\t -c \t Clean object and interface files \n\t -C \t Clean object, interface and main files \n\t -h \t Help"
	echo -e $help
esac

