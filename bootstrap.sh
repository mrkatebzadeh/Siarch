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

run_cmd() {
    cmd=$1
    msg=$2

    echo -e "${msg}: Started.."
    ${cmd}
    rc=$?
    if [[ $rc != 0 ]]; then
        echo -e "${RED}${CROSS_MARK}${msg}: Failed. ERR: ${rc}"
        exit $rc;
    else
        echo -e "${GREEN}${CHECK_MARK}${msg}: Done!"
    fi
}

run_cmd "sudo pacman -Sy --noconfirm" "Updating pacman"
run_cmd "sudo pacman -S git --noconfirm" "Installing git"
run_cmd "sudo pacman -S emacs --noconfirm" "Installing emacs"

run_cmd "git clone https://github.com/mrkatebzadeh/DotOrg.git" "Cloning DotOrg"
