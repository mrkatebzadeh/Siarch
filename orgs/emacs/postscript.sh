#!/usr/bin/env bash

DIRECTORY=$1
PACKAGE=$2

if [ ! -d ~/.emacs.d ]; then
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi
