# phdenzel's dotfiles

This is my collection of configuration files for a UNIX-based
operating system; a backup of settings, of sorts. In particular, it
features configurations for Emacs, zsh (and/or bash), XMonad (xorg),
and Hyprland (wayland).

Usually, these **dotfiles** are placed in the home folder (when
possible in `XDG_CONFIG_HOME=~/.config/`) where the corresponding
program looks for them, and overwrites potentially unwanted default
settings. So be careful when installing!  
I started this repo for macOS in order to make it feel more like Linux
(back when *macOS Sierra 10.12.2* and *macOS High Sierra 10.13.1* were
the latest versions). These days however, I'm back on Linux (*Arch*
and *Pop!_OS*). I am currently (slowly) working on conditionals to
automatically make everything completely compatible from the 'git-go',
so to speak... but for now all user-specifics are have to be manually
modified in `USERINFO` and copied into `~/.config/USERINFO`.

The repository can be cloned wherever you want; a natural place would
be `~/dotfiles`, i.e. the home folder.

**Warning**: Use with caution! Check the files first before
installing; especially all scripts in `installers/`.  The files in
`etc/` and `utils/` are probably no use to anyone except me, but I
included them anyways, just in case somebody wants to change them for
their own use.

### Example
![Screenshot of my shell prompt](imgs/screenshot.png)
  
### Install

For full functionality, make sure you have `zsh` installed. E.g. on Arch linux

```bash
sudo pacman -S zsh
chsh -s /bin/zsh
```

To install the dotfiles simply run:

```bash
./bootstrap.sh
```
while in `dotfiles` (use `bootstrap.sh -h` to see all options).

```
  Usage: bootstrap.sh [-c|--conf PATH] [-l|--bin-path PATH]
                      [-n|--dry-run] [-d|--diff] [-s|--skip]
                      [-b|--bin] [-e|--emacs] [-t|--themes]
                      [-x|--xmonad] [-w|--hypr]
                      [--source]

         Bootstrap script for installing the phdenzel/dotfiles using rsync.

         -h, --help         Prints this message.
         -c, --conf <path>  Sets the path where the configuration will be
                            installed. Default: $HOME/.config
         -l, --bin-path     Sets the path where the binaries will be installed,
                            resp. symlinked. Default: $HOME/local/bin
         -n, --dry-run      Runs all rsync commands in dry-run mode.
         -d, --diff         Shows all diffs between the already existing and
                            to-be-installed configurations;
                            only takes effect with the -n, --diff flag.
         -s, --skip         Skips installing all main dotfiles (when used with
                            e.g. -x, the script installs only the xmonad
                            configurations).
         -b, --bin          Installs/symlinks binaries from $HOME/dotfiles/bin
         -e, --emacs        Installs emacs configurations.
         -t, --themes       Installs the custom 'phd-dark' theme
                            (GTK, QT, highlight, etc.)
         -x, --xmonad       Installs xmonad configurations.
         -w, --hypr         Installs hyprland configurations.
         --source           Source the installed bash/zsh configuration.
```


### Shell scripts:
Moreover, the repository holds several shell scripts, although they
aren't strictly speaking dotfiles. Nevertheless, it's quite useful to
be able to clone a single repository onto a new machine and run only a
handful of commands that install all your scripts, settings and
libraries at once.  *Again, go through the files first and make sure
you know what they aare doing, before installing*.

* `arch.sh` - installs my most frequently used arch packages from the
  official and AUR repositories (use `arch.sh -h` for details)
* `arch_iso.sh` - downloads the latest iso of the arch installer
* `arch-install.org` - is a guide how to cleanly install a recent
  versions of arch
* `arch-setup.org` - is a guide of how I set up and configure arch
  from a clean install
* `arch_vm.sh` - initializes an arch VM using kvm/qemu
* `raspian.sh` - describes how I set up my RaspberryPi machines
* `macos.sh` - describes how I set up my macOS machines/vms
* `iso_macos*.sh` - describes how to convert macOS installers into
  isos

### More
[Mathias Bynens](https://github.com/mathiasbynens/dotfiles) has another great dotfile setup which was highly influencial to mine.
