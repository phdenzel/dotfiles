#!/bin/bash
####################################################### Bootstrap for dotfiles
# ATTENTION: This may overwrite some preexisting dotfiles in your home folder!
#            It is advisable to make a backup first...
# To be SOURCED in the dotfiles directory
# OPTIONS: --emacs   : install the emacs config files.
#          --bin     : install the binary files (as symlinks in ~/local/bin).
#          --terminal: install the terminal files (macOS-specific).
if [ "$1" == "--emacs" ]; then
    # Copy .emacs.d to its rightful place                                 
    if [ ! -d "${HOME}/.emacs.d/" ]; then
	      cp -r .emacs.d ${HOME}/
    else
        mv ${HOME}/.emacs.d ${HOME}/emacs.d-old
	      cp -r .emacs.d ${HOME}/
	      echo ".emacs.d already existed and has been backed up to ~/emacs.d-old"
    fi;
elif [ "$1" == "--terminal" ]; then
    # Copy the terminal files to Documents resp. Library/Serivices
    [ -d "${HOME}/Documents" ] && \
        cp etc/*.terminal ${HOME}/Documents/
    [ -d "${HOME}/Library/Services" ] && \
        cp -r etc/Launch\ Terminal.workflow ${HOME}/Library/Services/
elif [ "$1" == "--bin" ]; then
    # Link the binaries to ~/local/bin/
    mkdir -p ${HOME}/local/bin/  # don't forget to add to PATH
    ln -s $(pwd)/bin/* ${HOME}/local/bin/

elif [ "$1" == "--dry-run" ]; then
    rsync --exclude ".git/" \
          --exclude ".DS_Store" \
	        --exclude ".macOS" \
          --exclude "bootstrap.sh" \
          --exclude "screenshot.png" \
          --exclude "bin/" \
          --exclude "installers/" \
          --exclude "custom/" \
          --exclude "etc/" \
          --exclude "utils/" \
          --exclude "private/" \
	        --exclude ".emacs.d" \
          --exclude "matplotlibrc" \
          --exclude "LICENSE" \
          --exclude "README.md" \
          --dry-run -avh . ~;

    # mkdir -p ~/.matplotlib
    rsync --dry-run -avh matplotlibrc ~/.matplotlib/
    exit 1
    
else
    rsync --exclude ".git/" \
          --exclude ".DS_Store" \
	        --exclude ".macOS" \
          --exclude "bootstrap.sh" \
          --exclude "screenshot.png" \
          --exclude "bin/" \
          --exclude "installers/" \
          --exclude "custom/" \
          --exclude "etc/" \
          --exclude "utils/" \
          --exclude "private/" \
	        --exclude ".emacs.d" \
          --exclude "matplotlibrc" \
          --exclude "LICENSE" \
          --exclude "README.md" \
          -avh . ~;

    mkdir -p ~/.matplotlib
    rsync -avh matplotlibrc ~/.matplotlib/
fi;

source ~/.bashrc
