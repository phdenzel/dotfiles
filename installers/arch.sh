#/bin/bash
#
# Installer script for Archlinux
#     Note: this installer favours LEMP stack over LAMP
#
# @author: phdenzel
#
COLOR_RESET="\e[0m"
COLOR_RED="\e[1;38;5;001m"
COLOR_GREEN="\e[1;38;5;002m"
COLOR_BLUE="\e[1;38;5;004m"
PAC_BIN="sudo pacman"
YAY_BIN="yay"
INSTALL_YAY=0
DO_HEADLESS=0
DO_HYPR=0
DO_XMONAD=0
DRY_RUN=0
SKIP_AUR=0

read -r -d '' usage <<-EOF
  Usage: arch.sh [-y|--yay] [-h|--headless]
                 [-s|--skip-aur] [-n|--dry-run]
                 [-x|--xmonad] [-w|--hypr]
                 [--install-yay]

         Installer script for (minimal) Arch linux systems.

         -h, --help         Prints this message.
         -y, --yay          Exclusively use the yay package manager for
                            all installs.
         --headless         Install only packages for a headless system
                            (no graphical software).
         -s, --skip-aur     Install only packages from the official Arch linux
                            repositories, skipping AUR packages.
         -n, --dry-run      Simply prints the install commands, but does not
                            execute them.
         -x, --xmonad       Install xorg server, xorg utilities,
                            and xmonad dependencies.
         -w, --hypr         Install wayland compositor, wayland utilities,
                            and hyprland dependencies.
         --install-yay      Install the yay AUR helper.
EOF

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            echo "$usage"
            exit 0
            ;;
        --install-yay)
            INSTALL_YAY=1
            ;;
        -y|--yay)
            PAC_BIN=yay
            ;;
        --headless)
            DO_HEADLESS=1
            ;;
        -s|--skip-aur)
            SKIP_AUR=1
            ;;
        -n|--dry-run)
            DRY_RUN=1
            ;;
        -x|--xmonad)
            DO_XMONAD=1
            ;;
        -w|--hypr|--hyprland)
            DO_HYPR=1
            ;;
        *|-*|--*)
            echo "Unknown option $1"
            exit 1
            ;;
    esac
    shift
done

arch_pkgs=(
    xf86-video-intel
    nvidia nvidia-utils nvidia-settings
    fbset
    openssh
    rsync
    docker
    wireless_tools
    qt5ct
    qt6ct
    gtk-engine-murrine
    lxappearance
    emacs
    pyenv python-pip python-pipenv python-conda
    sddm
    accountsservice
    pcmanfm
    gvfs
    udisks2
    udiskie
    tumbler
    xarchiver
    ranger ueberzug highlight
    systemd-resolvconf
    zsh
    fzf
    bat
    eza
    zoxide
    fd
    ripgrep
    dust
    bash-completion
    man-db
    hunspell hunspell-en_us
    rofi
    xdotool
    xscreensaver
    chrony
    arch-install-scripts
    gptfdisk
    exfat-utils
    smartmontools
    zip unzip
    rsync
    cronie
    htop btop
    mtpfs
    gvfs-mtp gvfs-gphoto2
    vim
    base-devel
    gdb
    cmake
    cuda cuda-tools cudnn
    rust
    ruby rubygems
    jenv
    jdk-openjdk jdk8-openjdk
    texlive-basic texlive-bibtexextra texlive-bin texlive-fontsextra texlive-fontsrecommended texlive-formatsextra texlive-games texlive-humanities texlive-latex texlive-latexextra texlive-latexrecommended texlive-mathscience texlive-music texlive-pictures texlive-plaingeneric texlive-pstricks texlive-publishers
    biber
    gobject-introspection
    tk
    usbutils
    ethtool
    wol
    nfs-utils
    samba
    sshfs
    rclone
    firewalld
    fail2ban
    dnsmasq
    wireguard-tools
    nmap
    git
    wget
    curl
    transmission-cli transmission-gtk
    lynx
    w3m
    thunderbird
    nginx
    mariadb mariadb-clients mariadb-libs
    php php-cgi php-gd php-fpm php-intl php-imagick
    openssl
    isync
    pass pass-otp
    browserpass browserpass-chromium browserpass-firefox
    zbar
    libnotify
    colordiff
    qalculate-gtk
    xclip
    xsel
    scrot
    kdenlive
    breeze
    libreoffice-still
    gst-plugins-base gst-plugins-good gst-plugins-ugly
    zathura zathura-pdf-mupdf
    calibre
    lollypop
    mpd mpc ymuse
    easytag
    mpv
    celluloid
    mkvtoolnix-cli
    perl-image-exiftool
    zenity
    gimp
    inkscape
    openscad
    terminus-font
    ttf-dejavu
    ttf-fira-mono ttf-fira-sans
    ttf-roboto ttf-roboto-mono
    adobe-source-code-pro-fonts adobe-source-sans-fonts
    ttf-hack
    ttf-jetbrains-mono
    ttf-inconsolata
    ttf-ubuntu-font-family
    ttf-font-awesome
    noto-fonts-emoji
    virt-manager qemu-full edk2-ovmf vde2 ebtables dmidecode bridge-utils openbsd-netcat libguestfs
    openconnect networkmanager-openconnect
)

arch_headless_pkgs=(
    fbset
    openssh
    rsync
    docker
    emacs
    pyenv python-pip python-pipenv
    ranger ueberzug highlight
    systemd-resolvconf
    zsh
    fzf
    bat
    eza
    zoxide
    fd
    ripgrep
    dust
    bash-completion
    man-db
    hunspell hunspell-en_us
    chrony
    arch-install-scripts
    gptfdisk
    exfat-utils
    smartmontools
    zip unzip
    rsync
    cronie
    htop btop
    mtpfs
    vim
    base-devel
    gdb
    cmake
    rust
    ruby rubygems
    jenv jdk-openjdk jdk8-openjdk
    texlive-basic texlive-bibtexextra texlive-bin texlive-fontsextra texlive-fontsrecommended texlive-formatsextra texlive-games texlive-humanities texlive-latex texlive-latexextra texlive-latexrecommended texlive-mathscience texlive-music texlive-pictures texlive-plaingeneric texlive-pstricks texlive-publishers
    biber
    usbutils
    ethtool
    wol
    nfs-utils
    samba
    sshfs
    rclone
    firewalld
    fail2ban
    dnsmasq
    wireguard-tools
    nmap
    git
    wget
    curl
    transmission-cli
    lynx
    w3m
    nginx
    mariadb mariadb-clients mariadb-libs
    php php-cgi php-gd php-fpm php-intl php-imagick
    openssl
    isync
    pass pass-otp
    zbar
    colordiff
    libreoffice-still
    mkvtoolnix-cli
    perl-image-exiftool
    terminus-font
    ttf-fira-mono
    ttf-roboto-mono
    adobe-source-code-pro-fonts
    ttf-hack
    ttf-jetbrains-mono
    ttf-inconsolata
    ttf-ubuntu-font-family
    ttf-font-awesome
    virt-manager qemu-full edk2-ovmf vde2 ebtables dmidecode bridge-utils openbsd-netcat libguestfs
    openconnect networkmanager-openconnect
)

aur_pkgs=(
    alacritty-git
    brave-bin
    librewolf-bin
    gestures
    jmtpfs
    davfs2
    hdfview
    ds9-bin
    flac2all
    gwe
    mu
    cyrus-sasl-xoauth2-git
    oauth2token
    mailspring
    pass-secrets-git
    # ffmpeg-compat-57
    ttf-all-the-icons ttf-weather-icons
    ttf-monaco
    gk6x-bin
    jabref-git
    web-greeter
    zotero-bin
    slack-desktop
    zoom
    # work packages
    teams
    webex-bin
    openconnect-sso
)

aur_headless_pkgs=(
    alacritty-git
    jmtpfs
    davfs2
    flac2all
    mu
    cyrus-sasl-xoauth2-git
    oauth2token
    ffmpeg-compat-57
    ttf-all-the-icons ttf-weather-icons
    ttf-monaco
    sddm-theme-astronaut
)

xmonad_pkgs=(
    # build tools
    ghc ghc-static
    stack
    # prerequisites
    xorg-server xorg-apps xorg-xinit xorg-xmessage xorg-xrandr
    libx11 libxft libxinerama libxrandr libxss
    pkgconf
    picom         # compositor
    trayer        # system tray
    dunst         # notification daemon
    feh           # image viewer/wallpaper daemon
    volumeicon    # volume controls in status bar
    pavucontrol   # GUI for audio
    network-manager-applet  # GUI for network-manager
    blueman       # GUI for bluetoothctl
    arandr        # GUI for xrandr
)

hypr_pkgs=(
    hyprland-nvidia
    waybar        # status bar
    eww-git       # status bar
    foot          # alternate terminal
    xdg-desktop-portal-hyprland  # application portal, for screensharing
    pipewire wireplumber  # audio/video framework
    polkit-kde-agent  # system privilege control
    qt5-wayland qt6-wayland # Qt API for wayland
    hyprpaper     # wallpaper daemon
    dunst         # notification daemon
    wl-clipboard  # clipboard utilities
    cliphist      # clipboard history manager
    wtype         # xdotool for wayland
    satty-bin     # screenshot editor
    swayosd-git   # on-screen-display of system actions
    swayidle      # idle daemon for triggering dpms events
    swaylock-effects  # screen session locker (for security)
    pavucontrol   # graphical interface for audio
    network-manager-applet  # GUI for network-manager
    blueman       # GUI for bluetoothctl
    imv           # image viewer
    grim          # screenshot utility
    slurp         # region selector for grim
    hyprpicker    # color picker for wayland
    nwg-look      # graphical theme picker
    wev           # for debugging wayland events
    #nwg-displays  # graphical arandr alternative
)

if [[ $INSTALL_YAY -eq 1 ]]; then
    if command -v yay &> /dev/null; then
        echo "It seems $(command -v yay) is already installed..."
    elif [[ $DRY_RUN -eq 1 ]]; then
        echo "sudo pacman -S --needed git base-devel"
        echo "mkdir -p $HOME/local"
        echo "git clone https://aur.archlinux.org/yay.git $HOME/local/yay"
        echo "cd $HOME/local/yay"
        echo "makepkg -si"
    else
        sudo pacman -S --needed git base-devel \
            && mkdir -p $HOME/local \
            && git clone https://aur.archlinux.org/yay.git $HOME/local/yay \
            && cd $HOME/local/yay \
            && makepkg -si
    fi
fi

# sync and upgrade system
$PAC_BIN -Syyu
$YAY_BIN -Syyu

# install regular packages
if [[ $DO_HEADLESS -eq 1 ]]; then
    if [[ $DRY_RUN -eq 1 ]]; then
        echo "$PAC_BIN -S ${arch_headless_pkgs[@]}"
    else
        for pkgname in "${arch_headless_pkgs[@]}"; do
            if $PAC_BIN -Q $pkgname &>> /dev/null ; then
                echo -e "${COLOR_BLUE}$pkgname${COLOR_RESET} is already installed."
            else
                $PAC_BIN -S --noconfirm $pkgname
            fi
        done
    fi
else
    if [[ $DRY_RUN -eq 1 ]]; then
        echo "$PAC_BIN -S ${arch_pkgs[@]}"
    else
        for pkgname in "${arch_pkgs[@]}"; do
            if $PAC_BIN -Q $pkgname &>> /dev/null ; then
                echo -e "${COLOR_BLUE}$pkgname${COLOR_RESET} is already installed."
            else
                $PAC_BIN -S --noconfirm $pkgname
            fi
        done
    fi
fi

# install AUR packages
if [[ $SKIP_AUR -ne 1 ]]; then
    if [[ $DO_HEADLESS -eq 1 ]]; then
        if [[ $DRY_RUN -eq 1 ]]; then
            echo "$YAY_BIN -S ${aur_headless_pkgs[@]}"
        else
            for pkgname in "${aur_headless_pkgs[@]}"; do
                if $YAY_BIN -Q $pkgname &>> /dev/null ; then
                    echo -e "${COLOR_BLUE}$pkgname${COLOR_RESET} is already installed."
                else
                    $YAY_BIN -S --noconfirm $pkgname
                fi
            done
        fi
    else
        if [[ $DRY_RUN -eq 1 ]]; then
            echo "$YAY_BIN -S ${aur_pkgs[@]}"
        else
            for pkgname in "${aur_pkgs[@]}"; do
                if $YAY_BIN -Q $pkgname &>> /dev/null ; then
                    echo -e "${COLOR_BLUE}$pkgname${COLOR_RESET} is already installed."
                else
                    $YAY_BIN -S --noconfirm $pkgname
                fi
            done
        fi
    fi
fi

# install XMonad packages
if [[ $DO_XMONAD -eq 1 ]]; then
    if [[ $DRY_RUN -eq 1 ]]; then
        echo "$YAY_BIN -S ${xmonad_pkgs[@]}"
    else
        for pkgname in "${xmonad_pkgs[@]}"; do
            if $YAY_BIN -Q $pkgname &>> /dev/null ; then
                echo -e "${COLOR_BLUE}$pkgname${COLOR_RESET} is already installed."
            else
                $YAY_BIN -S --noconfirm $pkgname
            fi
        done
    fi
fi

# install Hyprland packages
if [[ $DO_HYPR -eq 1 ]]; then
    if [[ $DRY_RUN -eq 1 ]]; then
        echo "$YAY_BIN -S ${hypr_pkgs[@]}"
    else
        for pkgname in "${hypr_pkgs[@]}"; do
            if $YAY_BIN -Q $pkgname &>> /dev/null ; then
                echo -e "${COLOR_BLUE}$pkgname${COLOR_RESET} is already installed."
            else
                $YAY_BIN -S --noconfirm $pkgname
            fi
        done
    fi
fi
