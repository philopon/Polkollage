#!/bin/sh
export LD_LIBRARY_PATH=$PWD/lib
exec ./dist/build/Polkollage/Polkollage -p $PORT -e production --no-access-log --no-error-log
