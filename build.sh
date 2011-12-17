#!/bin/sh

mkdir .objs 2> /dev/null
cd src && ghc -fwarn-incomplete-patterns --make DietLISP.hs -outputdir ../.objs -o ../dlisp && cd ..

