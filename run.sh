#!/bin/sh
mkdir log
export LD_LIBRARY_PATH=$PWD/lib
./dist/build/Polkollage/Polkollage -p $PORT -e product
