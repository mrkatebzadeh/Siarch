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
Reboot and login to the new user and enter following command:

```
 startx
```

### Adding configs
```
./siarch.sh -s <app>
```
Config files of apps are under Siarch/dotfiles.
To add config files for all app, run:
```
./siarch.sh -S
```


## TODOs
- 🟢 Add mutt
- 🟢 Merge AUI into Siarch
- 🟢 Add greeting page
- 🟢 Remove unnecessary clone
- 🟢 Add option to refresh specific package
- 🟡 Add MacOS support
