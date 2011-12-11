#!/bin/sh

mkdir .objs 2> /dev/null
cd src && ghc --make DietLISP.hs -outputdir ../.objs -o ../dlisp && cd ..

