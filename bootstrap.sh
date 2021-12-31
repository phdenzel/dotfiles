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
BOOTSTRAP_PATH=$(dirname "$0")
BOOTSTRAP_PATH=$(cd "$BOOTSTRAP_PATH" && pwd)
EXCLUDES=(
    --exclude ".git/"
    --exclude ".macOS"
    --exclude "bootstrap.sh"
    --exclude "bin/"
    --exclude "installers/"
    --exclude "etc/"
    --exclude "imgs/"
    --exclude "utils/"
    --exclude "private/"
    --exclude ".emacs.d/"
    --exclude "LICENSE"
    --exclude "README.md"
    --exclude "phd-dark.tmTheme"
    #--exclude ".editorconfig"
)

if [ "$1" == "--emacs" ]; then
    # Copy .emacs.d to its rightful place                                 
    if [ ! -d "${HOME}/.emacs.d/" ]; then
	      cp -r .emacs.d ${HOME}/
    else
        mv ${HOME}/.emacs.d ${HOME}/emacs.d.bak
	      cp -r .emacs.d ${HOME}/
	      echo ".emacs.d already existed and has been backed up to ~/emacs.d.bak"
    fi;
elif [ "$1" == "--emacs-sync" ]; then
    rsync -ahv .emacs.d ~/.emacs.d
elif [ "$1" == "--bin" ]; then
    # Link the binaries to ~/local/bin/
    mkdir -p ${HOME}/local/bin/  # don't forget to add to PATH
    ln -s $(pwd)/bin/* ${HOME}/local/bin/
elif [ "$1" == "--dry-run" ]; then
    rsync "${EXCLUDES[@]}" --dry-run -avh . ~;
    exit 1
else
    rsync "${EXCLUDES[@]}" -avh . ~;
fi;

source ~/.bashrc
