#!/bin/sh
# Siarch, My Arch Dotfile manager
# M.R. Siavash Katebzadeh
# License: GNU GPLv3

### OPTIONS AND VARIABLES ###

progsfile=packs.csv
aurhelper=yay
name=${1:-siavash}

repodir="/home/$name/.local/src"; mkdir -p "$repodir"; chown -R "$name":wheel "$(dirname "$repodir")"

### FUNCTIONS ###
installpkg(){ pacman --noconfirm --needed -S "$1" >/dev/null 2>&1 ;}

error() { clear; printf "ERROR:\\n%s\\n" "$1" >&2; exit 1;}

manualinstall() { # Installs $1 manually if not installed. Used only for AUR helper here.
	[ -f "/usr/bin/$1" ] || (
	echo "Installing \"$1\", an AUR helper..." 
	cd /tmp || exit 1
	rm -rf /tmp/"$1"*
	curl -sO https://aur.archlinux.org/cgit/aur.git/snapshot/"$1".tar.gz &&
	sudo -u "$name" tar -xvf "$1".tar.gz >/dev/null 2>&1 &&
	cd "$1" &&
	sudo -u "$name" makepkg --noconfirm -si >/dev/null 2>&1
	cd /tmp || return 1) ;}

maininstall() { # Installs all needed programs from main repo.
	echo "Installing \`$1\` ($n of $total). $1 $2"
	installpkg "$1"
	}

gitmakeinstall() {
	progname="$(basename "$1" .git)"
	dir="$repodir/$progname"
	echo "Installing \`$progname\` ($n of $total) via \`git\` and \`make\`. $(basename "$1") $2"
	sudo -u "$name" git clone --depth 1 "$1" "$dir" >/dev/null 2>&1 || { cd "$dir" || return 1 ; sudo -u "$name" git pull --force origin master;}
	cd "$dir" || exit 1
	make >/dev/null 2>&1
	make install >/dev/null 2>&1
	cd /tmp || return 1 ;}

aurinstall() { \
	echo "Installing \`$1\` ($n of $total) from the AUR. $1 $2"
	echo "$aurinstalled" | grep -q "^$1$" && return 1
	sudo -u "$name" $aurhelper -S --noconfirm "$1"
	}

pipinstall() { \
	"Installing the Python package \`$1\` ($n of $total). $1 $2"
	[ -x "$(command -v "pip")" ] || installpkg python-pip >/dev/null 2>&1
	yes | pip install "$1"
	}

installationloop() { \
	([ -f "$progsfile" ] && cp "$progsfile" /tmp/progs.csv) || curl -Ls "$progsfile" | sed '/^#/d' > /tmp/progs.csv
	total=$(wc -l < /tmp/progs.csv)
	aurinstalled=$(pacman -Qqm)
	while IFS=, read -r tag program comment; do
		n=$((n+1))
		echo "$comment" | grep -q "^\".*\"$" && comment="$(echo "$comment" | sed "s/\(^\"\|\"$\)//g")"
		case "$tag" in
			"A") aurinstall "$program" "$comment" ;;
			"G") gitmakeinstall "$program" "$comment" ;;
			"P") pipinstall "$program" "$comment" ;;
			*) maininstall "$program" "$comment" ;;
		esac
	done < /tmp/progs.csv ;}

putgitrepo() { # Downloads a gitrepo $1 and places the files in $2 only overwriting conflicts
	"Downloading and installing config files..."
	[ -z "$3" ] && branch="master" || branch="$repobranch"
	dir=$(mktemp -d)
	[ ! -d "$2" ] && mkdir -p "$2"
	chown "$name":wheel "$dir" "$2"
	sudo -u "$name" git clone --recursive -b "$branch" --depth 1 --recurse-submodules "$1" "$dir" >/dev/null 2>&1
	sudo -u "$name" cp -rfT "$dir" "$2"
}

installall() {
	pacman -Sy --needed
	manualinstall $aurhelper || error "Failed to install AUR helper."
	installationloop
}
installall
