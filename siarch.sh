#!/bin/sh
# Siarch, My Arch Dotfile manager
# M.R. Siavash Katebzadeh
# License: GNU GPLv3

### OPTIONS AND VARIABLES ###

helpmessage() {
	printf "Arguments for custom use:\\n  -i: Install Siarch\\n  -s: install config\\n  -S: install all configs\\n -u: uninstall config\\n  -S: uninstall all configs\\n -h: Show this message\\n"
	exit 1
}

installpkg(){ pacman --noconfirm --needed -S "$1" >/dev/null 2>&1 ;}

error() { clear; printf "ERROR:\\n%s\\n" "$1" >&2; exit 1;}

while getopts ":s:u:r:o:hiSU" o; do case "${o}" in
	h) helpmessage ;;
	i) INSTALLALL=1 ;;
	s) STOW=${OPTARG} ;;
	S) STOWALL=1 ;;
	u) DELETESTOW=${OPTARG} ;;
	U) DELETEALL=1 ;;
	o) OUTPUTPATH=${OPTARG} ;;
	r) REMOVECONFIG=${OPTARG} ;;
	*) printf "Invalid option: -%s\\n" "$OPTARG" && exit 1 ;;
esac done

mkdir -p /etc/siarch

### FUNCTIONS ###

stow_config() {
	cd dotfiles
	config=$1
    echo $config
	stow -vvR $config -t ~
	cd -
}

stow_all() {
	cd dotfiles
	ls -d -- */ | xargs stow -v -t ~
	cd -
}

delete_config() {
	config=$1
	stow -v -D $config
}

delete_all() {
	ls -d -- */ | xargs stow -v -D
}



welcomemsg() { \
	echo "Welcome to Siarch Auto-Rice Bootstrapping Script!"
    echo "This script will automatically install a fully-featured Linux desktop, which I use as my main machine." 
	echo "Be sure the computer you are using has current pacman updates and refreshed Arch keyrings."
    echo "If it does not, the installation of some programs might fail."
	}

getuserandpass() { \
	# Prompts user for new username an password.
    echo "First, please enter a name for the user account."
	read name
	while ! echo "$name" | grep -q "^[a-z_][a-z0-9_-]*$"; do
		echo "Username not valid. Give a username beginning with a letter, with only lowercase letters, - or _."
        read name
	done
	echo "Enter a password for that user."
    read -s pass1
	echo "Retype password."
    read -s pass2
	while ! [ "$pass1" = "$pass2" ]; do
		unset pass2
		echo "Passwords do not match. Enter password again."
        read -s pass1
		echo "Retype password."
        read -s pass2
	done ;}

preinstallmsg() { \
	echo "The rest of the installation will now be totally automated, so you can sit back and relax."
    echo "It will take some time, but when done, you can relax even more with your complete system."
    echo "Now just press <Let's go!> and the system will begin installation!" 
	}

adduserandpass() { \
	# Adds user `$name` with password $pass1.
	echo "Adding user \"$name\"..."
	useradd -m -g wheel -s /bin/zsh "$name" >/dev/null 2>&1 ||
	usermod -a -G wheel "$name" && mkdir -p /home/"$name" && chown "$name":wheel /home/"$name"
    OUTPUTPATH="/home/$name/.siarch"
	siarchdir="/home/$name/.siarch"; mkdir -p "$siarchdir"; chown -R "$name":wheel "$(dirname "$siarchdir")"
	repodir="/home/$name/.local/src"; mkdir -p "$repodir"; chown -R "$name":wheel "$(dirname "$repodir")"
	sharedir="/home/$name/.local/share"; mkdir -p "$sharedir"; chown -R "$name":wheel "$(dirname "$sharedir")"
	bindir="/home/$name/.local/bin"; mkdir -p "$bindir"; chown -R "$name":wheel "$(dirname "$bindir")"
	configdir="/home/$name/.config"; mkdir -p "$configdir"; chown -R "$name":wheel "$(dirname "$configdir")"
	echo "$name:$pass1" | chpasswd
	unset pass1 pass2 ;
}

refreshkeys() { \
	echo "Refreshing Arch Keyring..."
	pacman -Q artix-keyring >/dev/null 2>&1 && pacman --noconfirm -S artix-keyring >/dev/null 2>&1
	pacman --noconfirm -S archlinux-keyring >/dev/null 2>&1
	}

newperms() { # Set special sudoers settings for install (or after).
	sed -i "/#Siarch/d" /etc/sudoers
	echo "$* #Siarch" >> /etc/sudoers ;}

putgitrepo() { # Downloads a gitrepo $1 and places the files in $2 only overwriting conflicts
	echo "Downloading and installing config files..."
    	dir="/home/$name/.siarch"
	cp -rT `pwd` $dir
	chown -R "$name":wheel "$dir"
	cd "$dir"
	runuser -l $name -c "cd .siarch; ./siarch.sh -S;"
	cd -
}

systembeepoff() { echo "Getting rid of that retarded error beep sound..." 
	rmmod pcspkr
	echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf ;}

finalize(){ \
	echo "Preparing welcome message..."
	echo "Congrats! Provided there were no hidden errors, the script completed successfully and all the programs and configuration files should be in place."
    echo "To run the new graphical environment, log out and log back in as your new user, then run the command \"startx\" to start the graphical environment (it will start automatically in tty1)." 
	}

installall() {

	pacman -Sy || error "Are you sure you're running this as the root user, are on an Arch-based distribution and have an internet connection?"

	# Welcome user.
	welcomemsg || error "User exited."

	# Get and verify username and password.
	getuserandpass || error "User exited."

	# Last chance for user to back out before install.
	preinstallmsg || error "User exited."


	# Refresh Arch keyrings.
	refreshkeys || error "Error automatically refreshing Arch keyring. Consider doing so manually."

	for x in curl base-devel git ntp zsh; do
		echo "Installing \`$x\` which is required to install and configure other programs."
		installpkg "$x"
	done

	echo "Synchronizing system time to ensure successful and secure installation of software..." 
	ntpdate 0.us.pool.ntp.org >/dev/null 2>&1

	adduserandpass || error "Error adding username and/or password."

	[ -f /etc/sudoers.pacnew ] && cp /etc/sudoers.pacnew /etc/sudoers # Just in case

	# Allow user to run sudo without password. Since AUR programs must be installed
	# in a fakeroot environment, this is required for all builds with AUR.
	newperms "%wheel ALL=(ALL) NOPASSWD: ALL"

	# Make pacman and yay colorful and adds eye candy on the progress bar because why not.
	grep -q "^Color" /etc/pacman.conf || sed -i "s/^#Color$/Color/" /etc/pacman.conf
	grep -q "ILoveCandy" /etc/pacman.conf || sed -i "/#VerbosePkgLists/a ILoveCandy" /etc/pacman.conf

	# Use all cores for compilation.
	sed -i "s/-j2/-j$(nproc)/;s/^#MAKEFLAGS/MAKEFLAGS/" /etc/makepkg.conf

    ./installer.sh $name

	echo  "Siarch Installation" --infobox "Finally, installing \`libxft-bgra\` to enable color emoji in suckless software without crashes."
	yes | sudo -u "$name" $aurhelper -S libxft-bgra-git >/dev/null 2>&1

	putgitrepo

	# Most important command! Get rid of the beep!
	systembeepoff

	# Make zsh the default shell for the user.
	chsh -s /bin/zsh "$name" >/dev/null 2>&1
	sudo -u "$name" mkdir -p "/home/$name/.cache/zsh/"

	# dbus UUID must be generated for Artix runit.
	dbus-uuidgen > /var/lib/dbus/machine-id

	# Fix fluidsynth/pulseaudio issue.
	grep -q "OTHER_OPTS='-a pulseaudio -m alsa_seq -r 48000'" /etc/conf.d/fluidsynth ||
		echo "OTHER_OPTS='-a pulseaudio -m alsa_seq -r 48000'" >> /etc/conf.d/fluidsynth

	# Start/restart PulseAudio.
	killall pulseaudio; sudo -u "$name" pulseaudio --start

	# This line, overwriting the `newperms` command above will allow the user to run
	# serveral important commands, `shutdown`, `reboot`, updating, etc. without a password.
	newperms "%wheel ALL=(ALL) ALL #Siarch
	%wheel ALL=(ALL) NOPASSWD: /usr/bin/shutdown,/usr/bin/reboot,/usr/bin/systemctl suspend,/usr/bin/wifi-menu,/usr/bin/mount,/usr/bin/umount,/usr/bin/nmtui,/usr/bin/pacman -Syu,/usr/bin/pacman -Syyu,/usr/bin/packer -Syu,/usr/bin/packer -Syyu,/usr/bin/systemctl restart NetworkManager,/usr/bin/rc-service NetworkManager restart,/usr/bin/pacman -Syyu --noconfirm,/usr/bin/loadkeys,/usr/bin/yay,/usr/bin/pacman -Syyuw --noconfirm"

	runuser -l $name -c "xdg-user-dirs-update"
	systemctl enable NetworkManager
	systemctl start NetworkManager
	systemctl enable dhcpcd
	systemctl start dhcpcd

	# Last message! Install complete!
	finalize
}


[[  "$INSTALLALL" = 1 ]] && installall
[ -n  "$STOW"  ] && stow_config $STOW
[[  "$STOWALL" = 1 ]] && stow_all
[ -n  "$DELETESTOW"  ] && delete_config $DELETESTOW
[[  "$DELETEALL" = 1 ]] && delete_all
exit 1
