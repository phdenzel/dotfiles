#!/bin/bash
####################################################### Bootstrap for dotfiles
# ATTENTION: This may overwrite some preexisting dotfiles in your home folder!
#            It is advisable to make a backup first...
# To be SOURCED or EXECUTED in the dotfiles directory
# Usage:
# . bootstrap.sh
# ./bootstrap.sh --emacs      : install the emacs config files.
#                --emacs-sync : sync the emacs config files.
#                --bin        : link the binary files (symlinks in ~/local/bin).
#                --gtk        : install custom gtk theme
BOOTSTRAP_PATH=$(dirname "$0")
BOOTSTRAP_PATH=$(cd "$BOOTSTRAP_PATH" && pwd)
CONF_HOME=${XDG_CONFIG_HOME:=$HOME/.config}
EXCLUDES=(
    --exclude ".git/"
    --exclude ".gitmodules"
    --exclude "bootstrap.sh"
    --exclude "bin/"
    --exclude "installers/"
    --exclude "etc/"
    --exclude "imgs/"
    --exclude "utils/"
    --exclude "private/"
    --exclude ".config/emacs/"
    --exclude ".config/gtk-3.0/"
    --exclude ".config/xmobar/xmobarconf/"
    --exclude "LICENSE"
    --exclude "README.md"
    --exclude ".themes/phd-dark.tmTheme"
    --exclude ".editorconfig"
)

if [ "$1" == "--emacs" ]; then
    # Copy .emacs.d to its rightful place
    echo "Installing emacs configs in $CONF_HOME/emacs"    
    if [ ! -d "$CONF_HOME/emacs" ]; then
	      cp -r .config/emacs $CONF_HOME/emacs
    else
        mv $CONF_HOME/emacs $CONF_HOME/emacs.bak
	      cp -r .config/emacs $CONF_HOME/
	      echo "$CONF_HOME/emacs directory already existed and has been backed up to $CONF_HOME/emacs.bak"
    fi;
elif [ "$1" == "--emacs-sync" ]; then
    rsync -ahv .config/emacs/ $CONF_HOME/emacs/
elif [ "$1" == "--bin" ]; then
    # Link the binaries to ~/local/bin/
    echo "Installing ~/local/bin/ symlink binaries"
    mkdir -p ${HOME}/local/bin/  # don't forget to add to PATH
    ln -s $BOOTSTRAP_PATH/bin/* ${HOME}/local/bin/
elif [ "$1" == "--gtk" ]; then
    echo "Installing gtk theme: phd-dark"
    rsync -ahv .config/gtk-3.0/ $CONF_HOME/gtk-3.0/
elif [ "$1" == "--dry-run" ]; then
    echo "Installation dry-run"
    rsync "${EXCLUDES[@]}" --dry-run -avh . ~;
    exit 0
else
    echo "Installing dotfiles to $HOME and $CONF_HOME"
    rsync "${EXCLUDES[@]}" -avh . ~;
    git submodule update
    if [ ! -d $CONF_HOME/xmobar/xmobarconf ]; then
        ln -s $(pwd)/.config/xmobar/xmobarconf $CONF_HOME/xmobar/
    fi;
    while true; do
	read -p "Source the new .bashrc file? [y/N] " answ
	case $answ in
		[Yy]* ) source $HOME/.bashrc; break;;
		* ) break;;
	esac;
    done;
fi;
