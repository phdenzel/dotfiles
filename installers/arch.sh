# Desktop
# this installer favours LEMP stack over LAMP
sudo pacman -Syyu
sudo pacman -S xf86-video-intel nvidia nvidia-utils nvidia-settings fbset openssh rsync docker pyenv ghc ghc-static qt5ct stack xorg-server xorg-apps xorg-xinit xorg-xmessage xorg-xrandr libx11 libxft libxinerama libxrandr libxss pkgconf wireless_tools picom feh trayer emacs python-pip python-pipenv gnome-keyring pcmanfm gvfs udisks2 xarchiver ranger ueberzug highlight systemd-resolvconf zsh fzf bat exa zoxide fd ripgrep dust bash-completion man-db hunspell hunspell-en_us xdotool xscreensaver network-manager-applet blueman volumeicon chrony arch-install-scripts gptfdisk exfat-utils zip unzip rsync cronie htop mtpfs gvfs-mtp gvfs-gphoto2 vim base-devel gdb cmake cuda cuda-tools cudnn rust ruby rubygems texlive-most texlive-lang gobject-introspection ethtool wol nfs-utils samba sshfs rclone firewalld fail2ban dnsmasq wireguard-tools nmap git wget curl transmission-cli transmission-gtk lynx w3m nginx mariadb mariadb-clients mariadb-libs php php-cgi php-gd php-fpm php-intl php-imagick openssl isync pass pass-otp zbar libnotify dunst colordiff qalculate-gtk xclip xsel scrot kdenlive breeze libreoffice-still zathura zathura-pdf-mupdf calibre lollypop easytag gst-plugins-base gst-plugins-good gst-plugins-ugly mpv celluloid mkvtoolnix-cli perl-image-exiftool zenity gimp inkscape openscad terminus-font ttf-dejavu ttf-fira-mono ttf-fira-sans ttf-roboto ttf-roboto-mono adobe-source-code-pro-fonts adobe-source-sans-fonts ttf-hack ttf-jetbrains-mono ttf-inconsolata ttf-ubuntu-font-family ttf-font-awesome virt-manager qemu-full edk2-ovmf vde2 ebtables dmidecode bridge-utils openbsd-netcat libguestfs openconnect networkmanager-openconnect

yay -S alacritty-git brave-bin mailspring jmtpfs davfs2 mu cyrus-sasl-xoauth2-git oauth2token ffmpeg-compat-57 ttf-all-the-icons ttf-weather-icons gk6x-bin


# Headless
# sudo pacman -S fbset openssh rsync pkgconf emacs python-pip python-pipenv ranger ueberzug highlight systemd-resolvconf zsh fzf bat exa zoxide fd ripgrep dust bash-completion man-db hunspell hunspell-en_us chrony arch-install-scripts gptfdisk exfat-utils zip unzip rsync cronie htop vim base-devel gdb cmake cuda cuda-tools cudnn rust ruby rubygems ethtool wol nfs-utils rclone firewalld fail2ban wireguard-tools nmap git wget curl transmission-cli lynx w3m nginx mariadb mariadb-clients mariadb-libs php php-cgi php-gd php-fpm php-intl php-imagick nextcloud openssl isync pass pass-otp zbar colordiff xclip xsel terminus-font ttf-dejavu ttf-fira-mono ttf-fira-sans ttf-roboto ttf-roboto-mono adobe-source-code-pro-fonts adobe-source-sans-fonts ttf-hack ttf-jetbrains-mono ttf-inconsolata ttf-ubuntu-font-family ttf-font-awesome

# yay ttf-all-the-icons ttf-weather-icons


# For work
yay -S slack-desktop teams webex-bin openconnect-sso
