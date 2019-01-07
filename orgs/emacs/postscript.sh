#!/usr/bin/env bash

DIRECTORY=$1
PACKAGE=$2

if [ ! -d ~/.emacs.d ]; then
    git clone -b develop https://github.com/hlissner/doom-emacs ~/.emacs.d
fi
mkdir -p ~/.doom.d/lisp
cp -r ./lisp/* ~/.doom.d/lisp
doom refresh
