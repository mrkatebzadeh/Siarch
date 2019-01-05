#!/usr/bin/env bash

DIRECTORY=$1
PACKAGE=$2

cd ${DIRECTORY}/dots/${PACKAGE}/.config/${PACKAGE}

chmod +x scope.sh commands.py
