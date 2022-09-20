#!/bin/zsh
####################################################### Bootstrap for dotfiles
# ATTENTION: This may overwrite some preexisting dotfiles in your home folder!
#            It is advisable to make a backup first...
# To be EXECUTED in the dotfiles directory
# Usage:
# ./bootstrap.sh [--dry-run]  : install most dotfiles in XDG_CONFIG_HOME
#                --emacs      : install the emacs config files.
#                --emacs-sync : sync the emacs config files.
#                --bin        : link the binary files (symlinks in ~/local/bin).
#                --themes     : install custom gtk/qt appearance and icon theme
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
    --exclude ".config/USERINFO"
    --exclude ".config/emacs/"
    --exclude ".themes/"
    --exclude ".icons/"
    --exclude ".config/gtk-3.0/"
    --exclude ".config/qt5ct/"
    --exclude ".config/xmonad/stack.yaml"
    --exclude ".config/xmobar/xmobarconf/"
    --exclude "vms/"
    --exclude "LICENSE"
    --exclude "README.md"
    --exclude ".themes/phd-dark.tmTheme"
    --exclude ".editorconfig"
)

if [ "$1" = "--emacs" ]; then
    # Copy .config/emacs to its rightful place
    echo "Installing emacs configs in $CONF_HOME/emacs"
    if [ ! -d "$CONF_HOME/emacs" ]; then
	      cp -r .config/emacs $CONF_HOME/emacs
    else
        mv $CONF_HOME/emacs $CONF_HOME/emacs.bak
	      cp -r .config/emacs $CONF_HOME/
	      echo "$CONF_HOME/emacs directory already existed and has been backed up to $CONF_HOME/emacs.bak"
    fi;
    if [ ! -d "$HOME/local/phd-modeline" ]; then
        git clone git@github.com:phdenzel/phd-modeline.git $HOME/local/phd-modeline
    fi
    if [ ! -d "$HOME/local/phd-dashboard" ]; then
        git clone git@github.com:phdenzel/phd-dashboard.git $HOME/local/phd-dashboard
    fi
    if [ ! -d "$HOME/local/phd-mu4e-setup" ]; then
        git clone git@github.com:phdenzel/phd-mu4e-setup.git $HOME/local/phd-mu4e-setup
    fi
elif [ "$1" = "--emacs-sync" ]; then
    rsync -ahv .config/emacs/ $CONF_HOME/emacs/
elif [ "$1" = "--bin" ]; then
    # Link the binaries to ~/local/bin/
    echo "Installing ~/local/bin/ symlink binaries"
    mkdir -p ${HOME}/local/bin/  # don't forget to add to PATH
    ln -s -f $BOOTSTRAP_PATH/bin/* ${HOME}/local/bin/
elif [ "$1" = "--themes" ]; then
    echo "Installing gtk/qt/sublime theme: phd-dark"
    rsync --exclude ".themes/phd-dark-highlight.theme" -ahv .themes/ $HOME/.themes/
    rsync -ahv .icons/ $HOME/.icons/
    rsync -ahv .config/gtk-3.0/ $CONF_HOME/gtk-3.0/
    rsync -ahv .config/qt5ct/ $CONF_HOME/qt5ct/
    sudo mkdir -p /usr/share/highlight/themes
    sudo cp .themes/phd-dark-highlight.theme /usr/share/highlight/themes/phd-dark.theme
    if command -v bat &> /dev/null; then
        mkdir -p "$(bat --config-dir)/themes"
        ln -s -f $HOME/.themes/phd-dark.tmTheme $(bat --config-dir)/themes/
        bat cache --build
    fi;
elif [ "$1" = "--dry-run" ]; then
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
	      read -q "?Source the new .zshrc file? [y/N] " answ
        echo ""
	      case $answ in
		        [Yy]* ) source $CONF_HOME/zshrc/.zshrc; break;;
		        * ) break;;
	      esac;
    done;
fi;
