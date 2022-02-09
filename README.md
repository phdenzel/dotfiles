# phdenzel's dotfiles

This is my collection of configuration files for a UNIX-based operating system; a backup of settings, of sorts.

Usually, these **dotfiles** are placed in the home folder where the according program looks for them, and overwrites potentially unwanted default settings. So be careful when installing!  
I started this repo for macOS in order to make it feel more like Linux (tested on *macOS Sierra 10.12.2* and *macos High Sierra 10.13.1*), however these days, I'm back on Linux (tested on *Pop!_OS* and *Arch*). I am currently (slowly) working on conditionals to automatically make everything completely compatible from the 'git-go', so to speak... but for now all user-specifics are have to be manually modified in `.config/USERINFO` and copied into `~/.config/USERINFO`.

The repository can be cloned wherever you want; a natural place would be `~/dotfiles`, i.e. the home folder.

**Warning**: Use with caution! Check the files first before installing; especially all scripts in `installers/`.
The files in `etc/` are probably no use to anyone except me, but I included them anyways, just in case somebody wants to change them for their own use.

### Example
![Screenshot of my shell prompt](imgs/screenshot.png)
  
### Install

To install the dotfiles simply type:

```bash
source bootstrap.sh
```
while in `dotfiles`.

Optionally, it is possible to run `bootstrap.sh` with flags, e.g. `--bin`, `--emacs`, or `--themes`.

```bash
./bootstrap.sh --bin
```
installs a `~/local/bin/` directory (which will be added to the environment `$PATH`) and links all the executables
in `bin/` to it.

Additionally, you can run:

```bash
./bootstrap.sh --emacs
```

which installs an emacs configuration: `.config/emacs` to the `$HOME` folder,   
or

```bash
source bootstrap.sh --themes
```

which installs:

* an appearance theme for GTK, QT, Unity, Cinnamon, etc.: `phd-dark`
* an icon theme: `phd-dark`


### Bash scripts:
Moreover, the repository holds several bash scripts, although they aren't strictly speaking dotfiles. Nevertheless, it's quite useful to be able to clone a single repository onto a new machine and run only a handful of commands that install all your settings and libraries at once.
*Again, go through the files first and make sure you know what they are doing, before installing*.

* `arch_iso.sh` - downloads the latest iso of the arch installer
* `arch-install.org` - is a guide how to cleanly install a recent versions of arch
* `arch-setup.org` - is a guide of how I set up arch from a clean install of arch
* `arch.sh` - installs my most frequently used arch packages from the official and AUR repositories
* `arch_vm.sh` - initializes an arch VM using kvm
* `raspian.sh` - describes how I set up my RaspberryPi machines
* `pop-os.sh` - describes how I set up my Pop!_OS machine
* `iso_macos*.sh` - describes how to convert macOS installers into isos
* `bash installers/emacs_install.sh` - downloads the source code for emacs, compiles it, and installs a symlink to `~/local/bin/`.
* `bash installers/gcc_install.sh` - downloads the source code for gcc, compiles it, and installs a symlink to `~/local/bin/`.

### More
[Mathias Bynens](https://github.com/mathiasbynens/dotfiles) has another great dotfile setup which was highly influencial to mine.
