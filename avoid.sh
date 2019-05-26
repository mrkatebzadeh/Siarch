#!/usr/bin/env bash
################################################################################
# Script      : Bootstrap
# Description : Installs the basics tools for arch
# Author      : Siavash Katebzadeh
# Email       : mr.katebzadeh@gmail.com
################################################################################

RED="\033[0;31m"
GREEN="\033[0;32m"
CHECK_MARK="✓"
CROSS_MARK="✗"

DOTS="dots"
SCRIPTS="scripts"
WALL="wall"

function loop_stow (){
for pkg in *;
do
	stow --adopt "$pkg"
done


}

cd dots
loop_stow

cd ..
stow --adopt scripts
stow --adopt wall

