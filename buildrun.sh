#!/bin/sh
./build.sh
if [ "$#" -eq 1 ]
then
	./plotter.sh $1
fi
if [ "$#" -eq 2 ]
then
	./plot.sh $1 $2
fi
