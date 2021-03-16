# What is Siarch?
It's my personal Arch installer/manager: Installing Arch using AUI script and
managing dotfiles using stow.
## Prerequisites
- Arch ISO
- Internet connection
## Download
Install git:

```
pacman -Sy git
```
Clone the script:

```
git clone https://github.com/mrkatebzadeh/Siarch.git
```
## Usage
### Installing Siarch

```
cd Siarch
./siarch.sh -i
```

and follow the instructions.
Reboot after finishing the installation.
## Installing Dot organizer
Use the added username in the following instrcutions:
```
./siarch.sh -U {username} -R -C
```

Reboot and login to the new user and enter following command:

```
 startx
```

## TODOs
- ✅ Add mutt
- ⭕ Merge AUI into Siarch
- ✅ Add greeting page
- ⭕ Remove unnecessary clone
- ✅ Add option to refresh specific package
