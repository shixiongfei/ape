#!/usr/bin/env sh

emcc -O2 ape.c ape_stdlib.c main.c -s WASM=1 -o ape.html --shell-file template.html

if [ ! -d "./bin/" ]; then
    mkdir ./bin
fi

mv ape.html ./bin/
mv ape.js ./bin/
mv ape.wasm ./bin/
