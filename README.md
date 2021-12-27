# phdenzel's dotfiles

This is my collection of configuration files for a UNIX-based operating system; a backup of settings, of sorts.

Usually, these **dotfiles** are placed in the home folder where the according program looks for them, and overwrites potentially unwanted default settings. So be careful when installing!  
Some of the files are especially designed for macOS (tested on *macOS Sierra 10.12.2* and *macos High Sierra 10.13.1*), however there are Linux/GNU-based variants to most of the commands (tested on *Red Hat Linux Server kernel-2.6.32-573.22.1.el6.x86_64*). I am currently (slowly) working on conditionals to automatically make other OS'es completely compatible from the 'git-go', so to speak...
  
The repository can be cloned wherever you want; a natural place would be `~/dotfiles`, i.e. the home folder, or to use a *symlink* from the clone to the home folder (that's what I do).

**Warning**: Use with caution! Check the files first before installing; especially `.macOS`, and all scripts in `installers/`.
The files in `custom/` are probably no use to anyone except me, but I included them anyways, just in case somebody wants to change them for their own use.

### Example
![Screenshot of my shell prompt](imgs/screenshot.png)
  
### Install

To install the dotfiles simply type:

```bash
source bootstrap.sh
```
while in `dotfiles`.

Optionally, it is possible to run `bootstrap.sh` with flags, e.g. `--bin`, `--emacs`, or `--terminal`.

```bash
source bootstrap.sh --bin
```
installs a `~/local/bin/` directory (which will be added to the environment `$PATH`) and links all the executables
in `bin/` to it.

Alternatively, you can run:

```bash
source bootstrap.sh --emacs
```

which installs an emacs configuration: `.emacs.d` to the `$HOME` folder,   
or

```bash
source bootstrap.sh --terminal
```

which installs:

* a terminal theme: `phd.terminal` to `~/Documents/`
* a custom-shortcut for macOS: `Launch\ Terminal.workflow` to `~/Library/Services/`

of course, these Terminal configurations are macOS-specific.   

### Bash scripts:
Moreover, the repository holds several bash scripts, although they aren't strictly speaking dotfiles. Nevertheless, it's quite useful to be able to clone a single repository onto a new machine and run only a handful of commands that install all your settings and libraries at once.
*Again, go through the files first and make sure you know what they are doing, before installing*.

* `./.macOS` - changes macOS-specific settings (only for macOS users)
* `bash installers/brew.sh` - downloads useful Homebrew packages (takes up quite a bit of disk space)
* `bash installers/pip.sh` - installs essential python libraries
* `bash installers/gem.sh` - installs a couple of ruby gems
* `bash installers/emacs_install.sh` - downloads the source code for emacs, compiles it, and installs a symlink to `~/local/bin/`.
* `bash installers/gcc_install.sh` - downloads the source code for gcc, compiles it, and installs a symlink to `~/local/bin/`.
* `bash installers/git-clones.sh` - clones useful git repositories into several locations within your home directory and its subdirectories.
* `bash custom/db2Documents.sh` - creates symlinks from `~/Dropbox` to `~/Documents` (fill the lists in the loops with your own directories or files if you want to use it)
* `bash custom/db2Home.sh` - creates symlinks from `~/Dropbox` to `~/` (fill the lists in the loops with your own directories or files if you want to use it)

### More
[Mathias Bynens](https://github.com/mathiasbynens/dotfiles) has another great dotfile setup which was highly influencial to mine.
