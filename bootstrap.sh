#!/bin/bash
####################################################### Bootstrap for dotfiles
# ATTENTION: This may overwrite some pre-existing dotfiles in your home folder!
#            It is advised to make a backup of your dotfiles first...
# This command must be run in the dotfiles directory!
BOOTSTRAP_PATH=$(cd $(dirname "$0") && pwd)
BIN_PATH=$HOME/local/bin
COLOR_RESET="\e[0m"
COLOR_RED="\e[1;38;5;001m"
COLOR_GREEN="\e[1;38;5;002m"
COLOR_BLUE="\e[1;38;5;004m"
CONF_HOME=${XDG_CONFIG_HOME:=$HOME/.config}
DO_BINS=0
DO_EMACS=0
DO_HYPR=0
DO_SKIP_CONF=0
DO_SOURCE=0
DO_THEMES=0
DO_XMONAD=0
DRY_RUN=0
FLAGS="avh"
DFLAGS=(
    --exclude ".config/emacs/"
    --exclude ".config/gtk-2.0/"
    --exclude ".config/gtk-3.0/"
    --exclude ".config/gtk-4.0/"
    --exclude ".config/qt5ct/"
    --exclude ".config/USERINFO"
    --exclude ".config/xmobar/"
    --exclude ".config/xmonad/"
    --exclude ".config/hypr/"
    --exclude ".editorconfig"
    --exclude ".fonts"
    --exclude ".git/"
    --exclude ".gitmodules"
    --exclude ".themes/"
    --exclude ".icons/"
    --exclude "bin/"
    --exclude "bootstrap.sh"
    --exclude "etc/"
    --exclude "imgs/"
    --exclude "installers/"
    --exclude "LICENSE"
    --exclude "README.md"
    --exclude "private/"
    --exclude "utils/"
    --exclude "vms/"
    --exclude ".DS_Store"
)
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            echo "Usage: bootstrap.sh [-c|--conf PATH] [-l|--bin-path PATH]"
            echo "          [-n|--dry-run] [-s|--skip] [--source]"
            echo "          [-b|--bin] [-e|--emacs] [-t|--themes]"
            echo "          [-w|--hypr] [-x|--xmonad]"
            echo ""
            echo "# BOOTSTRAP: $BOOTSTRAP_PATH"
            echo "# CONF:      $CONF_HOME"
            echo "# BIN_PATH:  $BIN_PATH"
            echo "# FLAGS:     -${FLAGS[@]}"
            echo ""
            exit 0
            ;;
        -c|--conf)
            shift
            CONF_HOME="$1"
            ;;
        -l|--bin-path)
            shift
            BIN_PATH="$1"
            ;;
        -n|--dry-run)
            DRY_RUN=1
            FLAGS+="n"
            ;;
        -s|--skip|--no-config)
            DO_SKIP_CONF=1
            ;;
        --source)
            DO_SOURCE=1
            ;;
        -b|--bin)
            DO_BINS=1
            ;;
        -e|--emacs)
            DO_EMACS=1
            ;;
        -t|--themes)
            DO_THEMES=1
            ;;
        -w|--hypr|--hyprland)
            DO_HYPR=1
            ;;
        -x|--xmonad)
            DO_XMONAD=1
            ;;
        *|-*|--*)
            echo "Unknown option $1"
            exit 1
            ;;
    esac
    shift
done

# Link the binaries to ~/local/bin/
if [[ $DO_BINS -eq 1 ]]; then
    echo "----------------------------------------------"
    echo -e "# Installing ${BIN_PATH} symlink ${COLOR_BLUE}binaries${COLOR_RESET}"
    echo "#    (Don't forget to add ${BIN_PATH} to PATH)"
    mkdir -p $BIN_PATH  # don't forget to add to PATH
    if [[ $DRY_RUN -ne 1 ]]; then
        ln -s -f $BOOTSTRAP_PATH/bin/* $BIN_PATH/
    else
        echo "ln -s -f ${BOOTSTRAP_PATH}/bin/* ${BIN_PATH}/"
    fi
fi;
# Install the emacs configuration
if [[ $DO_EMACS -eq 1 ]]; then
    echo "----------------------------------------------"
    echo -e "# Installing ${COLOR_BLUE}emacs${COLOR_RESET} configs in $CONF_HOME/emacs"
    if [[ $DRY_RUN -eq 1 ]]; then
        echo -n
    elif [[ -e "$CONF_HOME/emacs/phd-emacs.org" ]]; then
        echo -n
    elif [ -d "$CONF_HOME/emacs" ]; then
        mv $CONF_HOME/emacs $CONF_HOME/emacs.bak
        echo "$CONF_HOME/emacs directory already existed and has been backed up to $CONF_HOME/emacs.bak"
    fi
	  rsync "-${FLAGS[@]}" .config/emacs $CONF_HOME/
    if [ ! -d "$HOME/local/phd-modeline" ]; then
        git clone git@github.com:phdenzel/phd-modeline.git $HOME/local/phd-modeline
    fi
    if [ ! -d "$HOME/local/phd-dashboard" ]; then
        git clone git@github.com:phdenzel/phd-dashboard.git $HOME/local/phd-dashboard
    fi
    if [ ! -d "$HOME/local/phd-mu4e-setup" ]; then
        git clone git@github.com:phdenzel/phd-mu4e-setup.git $HOME/local/phd-mu4e-setup
    fi
fi;
# Install my custom theme
if [[ $DO_THEMES -eq 1 ]]; then
    echo "----------------------------------------------"
    echo -e "# Installing ${COLOR_BLUE}themes${COLOR_RESET}: phd-dark"
    rsync "-${FLAGS[@]}" --exclude ".themes/phd-dark-highlight.theme" .themes/ $HOME/.themes/
    echo -e "\n# Installing icons"
    rsync "-${FLAGS[@]}" .icons/ $HOME/.icons/
    echo -e "\n# Installing GTK configuration"
    # rsync "-${FLAGS[@]}" .config/gtk-2.0/ $CONF_HOME/gtk-2.0/
    # rsync "-${FLAGS[@]}" .config/gtk-3.0/ $CONF_HOME/gtk-3.0/
    rsync "-${FLAGS[@]}" .config/gtk-4.0/ $CONF_HOME/gtk-4.0/
    echo -e "\n# Installing qt configuration"
    rsync "-${FLAGS[@]}" .config/qt5ct/ $CONF_HOME/qt5ct/
    if [[ $DRY_RUN -ne 1 ]]; then
        sudo mkdir -p /usr/share/highlight/themes
        sudo cp .themes/phd-dark-highlight.theme /usr/share/highlight/themes/phd-dark.theme
        if command -v bat &> /dev/null; then
            mkdir -p "$(bat --config-dir)/themes"
            ln -s -f $HOME/.themes/phd-dark.tmTheme $(bat --config-dir)/themes/
            bat cache --build
        fi
    fi
fi;
# Install my hyprland configuration
if [[ $DO_HYPR -eq 1 ]]; then
    echo "----------------------------------------------"
    echo -e "# Installing ${COLOR_BLUE}hyprland${COLOR_RESET} configuration"
fi;
# Install my xmonad configuration
if [[ $DO_XMONAD -eq 1 ]]; then
    echo "----------------------------------------------"
    echo -e "# Installing ${COLOR_BLUE}xmonad${COLOR_RESET} configuration"
    rsync "-${FLAGS[@]}" --exclude ".config/xmonad/stack.yaml" .config/xmonad $CONF_HOME/
    rsync "-${FLAGS[@]}" --exclude ".config/xmobar/xmobarconf" .config/xmobar $CONF_HOME/
    if [[ $DRY_RUN -ne 1 ]]; then
        if [ ! -d $CONF_HOME/xmobar/xmobarconf ]; then
            ln -s -f $(pwd)/.config/xmobar/xmobarconf $CONF_HOME/xmobar/
        fi
    fi
fi;
# Install dotfiles
if [[ $DO_SKIP_CONF -ne 1 ]]; then
    echo "----------------------------------------------"
    echo -e "# Installing ${COLOR_BLUE}dotfiles${COLOR_RESET} to $HOME and $CONF_HOME"
    rsync "-${FLAGS[@]}" "${DFLAGS[@]}" $BOOTSTRAP_PATH/ ~;
    if [[ $DRY_RUN -ne 1 ]]; then
        chmod 700 ~/.gnupg
        git submodule update
    fi  
fi;
# Source the newly installed dotfiles
if [[ $DO_SOURCE -eq 1 ]]; then
    echo "----------------------------------------------"
    while true; do
        PROMPT=$'\e[1;38;5;004mSource\e[0m the new .zshrc file? [$\e[1;38;5;010my\e[0m/\e[1;38;5;005mN\e[0m] '
	      read -e -p "${PROMPT}" answ
        echo ""
	      case $answ in
		        [Yy]* ) source $CONF_HOME/zshrc/.zshrc; break;;
		        * ) break;;
	      esac;
    done;
fi;
