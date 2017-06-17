#!/bin/bash
####################################################### Bootstrap for dotfiles
# ATTENTION: This may overwrite some preexisting dotfiles in your home folder!
#            Best to make a backup first...
# To be executed in the dotfiles directory
# OPTIONS: --emacs   : copy the emacs config files.
#          --terminal: copy the terminal files (macOS-specific).
if [ "$1" == "--emacs" ]; then
    # Copy .emacs.d to its rightful place                                 
    if [ ! -d "${HOME}/.emacs.d/" ]; then
	      cp -r .emacs.d ${HOME}/
    else
        mv ${HOME}/.emacs.d ${HOME}/emacs.d-orig
	      cp -r .emacs.d ${HOME}/
	      echo ".emacs.d already existed and has been backed up to ~/emacs.d-orig"
    fi;
elif [ "$1" == "--terminal" ]; then
    # Copy the terminal files to Documents resp. Library/Serivices
    [ -d "${HOME}/Documents" ] && \
        cp etc/*.terminal ${HOME}/Documents/
    [ -d "${HOME}/Library/Services" ] && \
        cp -r etc/Launch\ Terminal.workflow/ ${HOME}/Library/Services/
elif [ "$1" == "--bin" ]; then
    # Copy the binaries to ~/ and link to ~/local/bin/
    cp github_repo ${HOME}/
    cp github_private_repo ${HOME}/
    mkdir -p ${HOME}/local/bin/  # don't forget to add to path
    ln -s ${HOME}/github_repo ${HOME}/local/bin/github_repo
    ln -s ${HOME}/github_private_repo ${HOME}/local/bin/github_private_repo
else
    rsync --exclude ".git/" \
          --exclude ".DS_Store" \
          --exclude "bootstrap.sh" \
          --exclude "github_repo" \
          --exclude "github_private_repo " \
          --exclude "screenshot.png" \
          --exclude "etc/" \
          --exclude "utils/" \
          --exclude "private/" \
	        --exclude ".emacs.d" \
          --exclude "README.md" \
          -avh . ~;
fi;

source ~/.bashrc
