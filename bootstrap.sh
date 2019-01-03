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

BABELCMD="emacs --batch -l org %s -f org-babel-tangle"
PACMANCMD="sudo pacman -S %s --noconfirm"
GITHUBPATH="https://github.com/mrkatebzadeh/Dotfiles.git"

DIRECTORY=~/.dotorg

run_cmd() {
    cmd=$1
    msg=$2

    echo -e "${msg}: Started.."
    ${cmd}
    rc=$?
    if [[ $rc != 0 ]]; then
        echo -e "${RED}${CROSS_MARK} ${msg}: Failed. ERR: ${rc}"
        exit $rc;
    else
        echo -e "${GREEN}${CHECK_MARK} ${msg}: Done!"
    fi
}

install_by_pacman() {
    package=$1
    run_cmd "${PACMANCMD/\%s/${package}}" "Pacman: Installing ${package}"
}

install_all() {
    run_cmd "sudo pacman -Sy --noconfirm" "Updating pacman"
    packages=( git emacs stow )
    for package in "${packages[@]}" ; do
        install_by_pacman ${package}
    done
    run_cmd "stow stow" "Stowing stow"
}

tangle_all() {
    if [ ! -d "$DIRECTORY" ]; then
        run_cmd "git clone ${GITHUBPATH} ${DIRECTORY}" "Cloning Dotfiles"
    fi
    run_cmd "cd ${DIRECTORY}" "Going to ${DIRECTORY}"
    for dir in $( ls -d orgs/* ); do
        cd ${dir}
        for org in $( ls *.org ); do
            run_cmd "${BABELCMD/\%s/${org}}" "Tangling ${dir}/${org}"
        done
        cd ${DIRECTORY}
    done
}

stow_all() {
    cd ${DIRECTORY}
    cd dots

    for dir in $( ls  ); do
        run_cmd "stow ${dir}" "Stowing ${dir}"
    done

}

refresh() {
    tangle_all
    stow_all
}

display_usage() {
    echo -e "\nUsage:\n$0 [arguments] \n"
    echo -e "\t-i\tInstall all required packages."
    echo -e "\t-t\tTangle all org files."
    echo -e "\t-s\tStow all dotfiles."
    echo -e "\t-r\tTangle and stow all config files."
    echo -e "\t-a\tInstall all packages, tangle and stow all config files."
}

while getopts ":irstha" opt; do
  case $opt in
    h) display_usage; exit 1 ;;
    i) install_all ;;
    r) tangle_all; stow_all ;;
    s) stow_all ;;
    t) tangle_all ;;
    a) install_all; tangle_all; stow_all ;;
    *) display_usage; exit 1 ;;
  esac
done
