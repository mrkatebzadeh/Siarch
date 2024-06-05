#!/usr/bin/env bash

NIX_DAEMON=/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

install_nix() {
	echo "Installing Nix..."
	curl -L https://nixos.org/nix/install | sh -s -- --daemon --yes
	if [ $? -ne 0 ]; then
		echo "Nix installation failed!"
		exit 1
	fi

	. $NIX_DAEMON

	echo "Configuring Nix..."
	mkdir -p $HOME/.config/nix
	echo "experimental-features = nix-command flakes" >$HOME/.config/nix/nix.conf

	echo "Nix installed and configured with nix-command and flakes features."
}

check_nix_installed() {
	if [ -f $NIX_DAEMON ] ; then
		echo "Nix is already installed."
		. $NIX_DAEMON
		return 0
	else
		return 1
	fi
}

install_home_manager() {
	if [[ "$OSTYPE" == "linux-gnu"* ]]; then
		echo "Installing Home Manager..."
		nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
		nix-channel --update
		nix-shell '<home-manager>' -A install
		if [ $? -ne 0 ]; then
			echo "Home Manager installation failed!"
			exit 1
		fi

		echo "Home Manager installed successfully."
	fi
}

check_home_manager_installed() {
	if command -v home-manager >/dev/null 2>&1; then
		echo "Home Manager is already installed."
		return 0
	else
		return 1
	fi
}

rebuild_configuration() {
	if [[ "$OSTYPE" == "linux-gnu"* ]]; then
		echo "Rebuilding configuration with Nix on Linux..."
		nix run home-manager -- switch --flake ".#$1"
	elif [[ "$OSTYPE" == "darwin"* ]]; then
		echo "Rebuilding configuration with Nix on macOS..."
		nix run nix-darwin -- switch --flake ".#$1"
	else
		echo "Unsupported OS: $OSTYPE"
		exit 1
	fi
}

check_nix_installed || install_nix

check_home_manager_installed || install_home_manager

rebuild_configuration $1
