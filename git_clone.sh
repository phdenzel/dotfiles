#!/bin/bash
####################################################### Git clone installer
### Set up github home folder
if [ ! -d "${HOME}/git-clones" ]; then
    mkdir git-clones;
cd git-clones;
### Google fonts (for Fira Mono and Roboto Mono)
git clone git@github.com:google/fonts.git

### Emacs icons
git clone git@github.com:domtronn/all-the-icons.el.git
