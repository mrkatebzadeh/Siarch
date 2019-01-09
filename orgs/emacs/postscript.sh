#!/usr/bin/env bash

DIRECTORY=$1
PACKAGE=$2

if [ ! -d ~/.emacs.d ]; then
    git clone -b develop https://github.com/hlissner/doom-emacs ~/.emacs.d
fi
mkdir -p ~/.config/doom/lisp
cp -r ./lisp/* ~/.config/doom/lisp
~/.emacs.d/bin/doom refresh -y
