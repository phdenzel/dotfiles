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
    # Link the binaries to ~/local/bin/
    mkdir -p ${HOME}/local/bin/  # don't forget to add to PATH
    ln -s $(pwd)/github_repo ${HOME}/local/bin/
    ln -s $(pwd)/github_private_repo ${HOME}/local/bin/
    ln -s $(pwd)/free ${HOME}/local/bin/
    ln -s $(pwd)/syncExt ${HOME}/local/bin/
    ln -s $(pwd)/overleaf_push ${HOME}/local/bin/
    
else
    rsync --exclude ".git/" \
          --exclude ".DS_Store" \
          --exclude "bootstrap.sh" \
          --exclude "github_repo" \
          --exclude "github_private_repo" \
          --exclude "free" \
          --exclude "syncExt" \
          --exclude "overleaf_push" \
          --exclude "screenshot.png" \
          --exclude "etc/" \
          --exclude "utils/" \
          --exclude "private/" \
	        --exclude ".emacs.d" \
          --exclude "LICENSE" \
          --exclude "README.md" \
          -avh . ~;
fi;

source ~/.bashrc
