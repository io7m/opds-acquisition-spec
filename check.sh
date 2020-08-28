#!/bin/sh -ex

rm -f Stack.hi Stack.o
rm -f OPDS.hi OPDS.o

ghc -dynamic -c -W -Wall -Werror Stack.hs
exec ghc -dynamic -c -W -Wall -Werror -pgmL markdown-unlit OPDS.lhs
