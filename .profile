#!/bin/sh
####################################################### profile

####################################################### Source the necessary dotfiles
# Note: use shell/xauto to automatically start xorg-server when logging in
for file in ${XDG_CONFIG_HOME:=$HOME/.config}/{shell/path,shell/colors,shell/exports,shell/functions,shell/aliases}; do
    [ -r "$file" ] && [ -f "$file" ] && . "$file"
done;
unset file;

####################################################### END profile
