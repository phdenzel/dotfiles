#!/bin/bash
####################################################### Git clone installer
### Set up github home folder
if [ ! -d "${HOME}/git-clones" ]; then
    mkdir $HOME/git-clones;
cd $HOME/git-clones;
### Google fonts (for Fira Mono and Roboto Mono)
git clone git@github.com:google/fonts.git

### Emacs icons
git clone git@github.com:domtronn/all-the-icons.el.git

### Org mode themes
git clone git@github.com:fniessen/org-html-themes.git
