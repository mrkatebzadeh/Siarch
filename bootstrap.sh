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
PACMANCMD="sudo pacman -S %s --noconfirm --needed"
GITHUBPATH="https://github.com/mrkatebzadeh/Dotfiles.git"
PROGSFILE="progs.csv"
AURHELPER="yay"

DIRECTORY=.dotorg
USERNAME=`whoami`
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

tangle_all() {
    if [ ! -d "/home/$USERNAME/$DIRECTORY" ]; then
        run_cmd "git clone ${GITHUBPATH} /home/$USERNAME/${DIRECTORY}" "Cloning Dotfiles"
    fi
    run_cmd "cd /home/$USERNAME/${DIRECTORY}" "Going to ${DIRECTORY}"
    for dir in $( ls -d orgs/* ); do
        cd ${dir}
        for org in $( ls *.org ); do
            run_cmd "${BABELCMD/\%s/${org}}" "Tangling ${dir}/${org}"
        done
        cd /home/$USERNAME/${DIRECTORY}
    done
}

stow_all() {
    echo "--target=/home/$USERNAME" > ~/.stowrc
    cd /home/$USERNAME
    echo "--target=/home/$USERNAME" > .stowrc
    cd /home/$USERNAME/${DIRECTORY}

    stow script
    stow wall
    wal -c -i wall/.config/wall.jpg
    cd dots


    for dir in $( ls  ); do
        run_cmd "stow ${dir}" "Stowing ${dir}"
    done

}

postscript_all() {

    run_cmd "cd /home/$USERNAME/${DIRECTORY}/orgs" "Going to ${DIRECTORY}/orgs"
    for dir in $( ls -d * ); do
        cd ${dir}
        for script in $( ls *.sh 2> /dev/null ); do
            run_cmd "bash ${script} /home/$USERNAME/${DIRECTORY} ${dir}" "Postscripting ${dir}"
        done
        cd /home/$USERNAME/${DIRECTORY}/orgs
    done
}

update() {
    git pull origin master
}

display_usage() {
    echo -e "\nUsage:\n$0 [arguments] \n"
    echo -e "\t-i\tInstall all required packages."
    echo -e "\t-t\tTangle all org files."
    echo -e "\t-s\tStow all dotfiles."
    echo -e "\t-r\tTangle and stow all config files."
    echo -e "\t-p\tRun postscripts for each package, if it exists."
    echo -e "\t-a\tInstall all packages, tangle and stow all config files."
    echo -e "\t-u\tUpdate Siarch."
}

initialcheck() {
    pacman -Syyu --noconfirm --needed dialog || { echo \
        "Are you sure you're running this as the root user? Are you sure you're using an Arch-based distro? ;-) Are you sure you have an internet connection? Are you sure your Arch keyring is updated?"; exit; }
}

preinstallmsg() {
    dialog --title "Do you want to start the installation?" --yes-label "Yes" \
        --no-label "No" --yesno \
        "Starting the installation" 13 60 || { clear; exit; }
}

welcomemsg() {
	dialog --title "Siarch" --msgbox "Welcome to the Siarch installation." 10 60
}

refreshkeys() {
	dialog --infobox "Refreshing Arch Keyring..." 4 40
	run_cmd \
        "pacman --noconfirm -Sy archlinux-keyring" \
        "Refreshing Arch Keyring"
}

getuserandpass() {
	name=$(dialog --inputbox \
        "Please enter a name for the user account." 10 60 3>&1 1>&2 2>&3 3>&1) \
        || exit
	namere="^[a-z_][a-z0-9_-]*$"
	while ! [[ "${name}" =~ ${namere} ]]; do
		name=$(dialog --no-cancel --inputbox \
            "Username isn't not valid. Give a username beginning with a letter, with only lowercase letters, - or _." 10 60 3>&1 1>&2 2>&3 3>&1)
	done
    USERNAME=$name
	pass1=$(dialog --no-cancel --passwordbox \
        "Enter a password." 10 60 3>&1 1>&2 2>&3 3>&1)
	pass2=$(dialog --no-cancel --passwordbox \
        "Retype password." 10 60 3>&1 1>&2 2>&3 3>&1)
	while ! [[ ${pass1} == "${pass2}" ]]; do
		unset pass2
		pass1=$(dialog --no-cancel --passwordbox \
            "Passwords do not match.\\n\\nEnter password again." \
            10 60 3>&1 1>&2 2>&3 3>&1)
		pass2=$(dialog --no-cancel --passwordbox \
            "Retype password." 10 60 3>&1 1>&2 2>&3 3>&1)
	done ;
}

usercheck() {
	! (id -u "$name" >/dev/null) 2>&1 ||
	    dialog --colors --title "WARNING!" --yes-label "CONTINUE" \
            --no-label "No wait..." --yesno \
            "The user \`${name}\` already exists on this system. Siarch can install for a user already existing, but it will \\Zboverwrite\\Zn any conflicting settings/dotfiles on the user account.\\n\\nSiarch will \\Zbnot\\Zn overwrite your user files, documents, videos, etc., so don't worry about that, but only click <CONTINUE> if you don't mind your settings being overwritten.\\n\\nNote also that Siarch will change ${name}'s password to the one you just gave." 14 70
}

adduserandpass() {
	dialog --infobox "Adding user \"$name\"..." 4 50
	useradd -m -g wheel -s /bin/bash "$name" >/dev/null 2>&1 ||
	usermod -a -G wheel "$name" && mkdir -p /home/"$name" && chown "$name":wheel /home/"$name"
	echo "$name:$pass1" | chpasswd
	unset pass1 pass2
}

gitmakeinstall() {
	dir=$(mktemp -d)
	dialog --title "Siarch Installation" --infobox "Installing \`$(basename "$1")\` ($n of $total) via \`git\` and \`make\`. $(basename "$1") $2" 5 70
	git clone --depth 1 "$1" "$dir" >/dev/null 2>&1
	cd "$dir" || exit
	make >/dev/null 2>&1
	make install >/dev/null 2>&1
	cd /tmp || return
}

maininstall() {
	dialog --title "Siarch Installation" --infobox "Installing \`$1\` ($n of $total). $1 $2" 5 70
	pacman --noconfirm --needed -S "$1" >/dev/null 2>&1
}

aurinstall() {
	dialog --title "Siarch Installation" --infobox "Installing \`$1\` ($n of $total) from the AUR. $1 $2" 5 70
	grep "^$1$" <<< "$aurinstalled" && return
	sudo -u "$name" $AURHELPER -S --noconfirm "$1" >/dev/null 2>&1
}

installationloop() {
	([ -f "$PROGSFILE" ] && cp "$PROGSFILE" /tmp/progs.csv) || curl -Ls "$PROGSFILE" | sed '/^#/d' > /tmp/progs.csv
	total=$(wc -l < /tmp/progs.csv)
	aurinstalled=$(pacman -Qm | awk '{print $1}')
	while IFS=, read -r tag program comment; do
	n=$((n+1))
	echo "$comment" | grep "^\".*\"$" >/dev/null && comment="$(echo "$comment" | sed "s/\(^\"\|\"$\)//g")"
	case "$tag" in
	"") maininstall "$program" "$comment" ;;
	"A") aurinstall "$program" "$comment" ;;
	"G") gitmakeinstall "$program" "$comment" ;;
	esac
	done < /tmp/progs.csv
}

serviceinit() {
    for service in "$@"; do
	dialog --infobox "Enabling \"$service\"..." 4 40
	systemctl enable "$service"
	systemctl start "$service"
	done
}

newperms() {
	sed -i "/#DOTORG/d" /etc/sudoers
	echo -e "$@ #DOTORG" >> /etc/sudoers
}

systembeepoff() {
    dialog --infobox "Getting rid of that retarded error beep sound..." 10 50
	rmmod pcspkr
	echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf
}

putgitrepo() {
	dialog --infobox "Downloading and installing config files..." 4 60
	dir=$(mktemp -d)
	chown -R "$name":wheel "$dir"
	sudo -u "$name" git clone --depth 1 "$1" "$dir"/gitrepo >/dev/null 2>&1 &&
	sudo -u "$name" mkdir -p "$2" &&
	sudo -u "$name" cp -rT "$dir"/gitrepo "$2"
}

resetpulse() {
    dialog --infobox "Reseting Pulseaudio..." 4 50
	killall pulseaudio
	sudo -n "$name" pulseaudio --start
}

manualinstall() {
	[[ -f /usr/bin/$1 ]] || (
	dialog --infobox "Installing \"$1\", an AUR helper..." 4 50
	cd /tmp || exit
	rm -rf /tmp/"$1"*
	curl -sO https://aur.archlinux.org/cgit/aur.git/snapshot/"$1".tar.gz &&
	sudo -u "$name" tar -xvf "$1".tar.gz >/dev/null 2>&1 &&
	cd "$1" &&
	sudo -u "$name" makepkg --noconfirm -si >/dev/null 2>&1
	cd /tmp || return)
}

finalize(){
	dialog --infobox "Preparing welcome message..." 4 50
	echo "exec_always --no-startup-id notify-send -i ~/.scripts/pix/siarch.png '<b>Welcome to Siarch:</b> Press Super+F1 for the manual.' -t 10000"  >> "/home/$name/.config/i3/config"
	dialog --title "All done!" --msgbox "Congrats! Provided there were no hidden errors, the script completed successfully and all the programs and configuration files should be in place.\\n\\nTo run the new graphical environment, log out and log back in as your new user, then run the command \"startx\" to start the graphical environment (it will start automatically in tty1).\\n\\n.t Luke" 12 80
}

install_all() {
    run_cmd "sudo pacman -Sy --noconfirm" "Updating pacman"

    initialcheck

    welcomemsg || { clear; exit; }

    preinstallmsg || { clear; exit; }

    packages=( git emacs stow base-devel )
    for package in "${packages[@]}" ; do
        install_by_pacman ${package}
    done
#    run_cmd "stow stow" "Stowing stow"

    getuserandpass

    adduserandpass

    refreshkeys

    newperms "%wheel ALL=(ALL) NOPASSWD: ALL"

    manualinstall $AURHELPER

    installationloop

#    putgitrepo "$GITHUBPATH" "/home/$name/.siarch"

#    putgitrepo "https://github.com/LukeSmithxyz/mozillarbs.git" "/home/$name/.mozilla/firefox"


    [ -f /usr/bin/pulseaudio ] && resetpulse

#    dialog --infobox "Installing vim plugins..." 4 50
#    (sleep 30 && killall vim) &
#    sudo -u "$name" vim -E -c "PlugUpdate|visual|q|q" >/dev/null

    serviceinit NetworkManager cronie

    newperms "%wheel ALL=(ALL) ALL\\n%wheel ALL=(ALL) NOPASSWD: /usr/bin/shutdown,/usr/bin/reboot,/usr/bin/systemctl suspend,/usr/bin/wifi-menu,/usr/bin/mount,/usr/bin/umount,/usr/bin/pacman -Syu,/usr/bin/pacman -Syyu,/usr/bin/packer -Syu,/usr/bin/packer -Syyu,/usr/bin/systemctl restart NetworkManager,/usr/bin/rc-service NetworkManager restart,/usr/bin/pacman -Syyu --noconfirm,/usr/bin/loadkeys,/usr/bin/yay,/usr/bin/pacman -Syyuw --noconfirm"

    sed -i "s/^#Color/Color/g" /etc/pacman.conf

    finalize

    clear
}

while getopts ":irsthapugIU:" opt; do
    case $opt in
    I) cd arch || exit ; ./fifo ;;
    h) display_usage; exit 1 ;;
    i) install_all ;;
    r) tangle_all; postscript_all; stow_all ;;
    s) stow_all ;;
    t) tangle_all ;;
    p) postscript_all ;;
    u) update ;;
    a) install_all; tangle_all; postscript_all; stow_all ;;
	g) GITHUBPATH=${OPTARG} && git ls-remote "${GITHUBPATH}" || exit ;;
	P) PROGSFILE=${OPTARG} ;;
	A) AURHELPER=${OPTARG} ;;
	U) USERNAME=${OPTARG} ;;
    *) display_usage; exit 1 ;;
  esac
done
