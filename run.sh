#!/bin/sh
mkdir log
export LD_LIBRARY_PATH=$PWD/lib
exec ./dist/build/Polkollage/Polkollage -p $PORT -e product
