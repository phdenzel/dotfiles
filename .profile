#!/bin/sh
####################################################### profile

####################################################### Source the necessary dotfiles
# Note: use shell/xauto to automatically start xorg-server when logging in
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
for file in $XDG_CONFIG_HOME/{shell/functions,shell/exports,shell/path,shell/colors,shell/aliases}; do
    [ -r "$file" ] && [ -f "$file" ] && . "$file"
done;
unset file;

####################################################### END profile
