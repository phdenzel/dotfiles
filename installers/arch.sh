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
DO_HEADLESS=0
DO_HYPR=0
DO_XMONAD=0
DRY_RUN=0
SKIP_AUR=0

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            echo "Usage: arch.sh [-y|--yay] [-h|--headless]"
            echo "          [-s|--skip-aur] [-n|--dry-run]"
            echo "          [-x|--xmonad] [-w|--hypr]"
            echo ""
            echo ""
            exit 0
            ;;
        -y|--yay)
            PAC_BIN=yay
            ;;
        -h|--headless)
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
    lxappearance
    emacs
    pyenv python-pip python-pipenv
    lightdm
    accountsservice
    pcmanfm
    gvfs
    udisks2
    xarchiver
    ranger ueberzug highlight
    systemd-resolvconf
    zsh
    fzf
    bat
    exa
    zoxide
    fd
    ripgrep
    dust
    bash-completion
    man-db
    hunspell hunspell-en_us
    dunst
    pavucontrol
    network-manager-applet
    blueman
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
    nginx
    mariadb mariadb-clients mariadb-libs
    php php-cgi php-gd php-fpm php-intl php-imagick
    openssl
    isync
    pass pass-otp
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
    exa
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
    ffmpeg-compat-57
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
    web-greeter
)

xmonad_pkgs=(
    ghc ghc-static
    stack
    xorg-server xorg-apps xorg-xinit xorg-xmessage xorg-xrandr
    libx11 libxft libxinerama libxrandr libxss
    pkgconf
    picom
    trayer
    feh
    volumeicon
)

hypr_pkgs=(
    hyprland-nvidia
    eww-wayland
    tofi
    xdg-desktop-portal-hyprland
    dunst
    pipewire wireplumber
    polkit-kde-agent
    qt5-wayland qt6-wayland
    hyprpaper
    nwg-look
)

# sync and upgrade system
$PAC_BIN -Syyuw
$YAY_BIN -Syyuw

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
