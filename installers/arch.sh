### Arch setup (execute all cmds w/ sudo)
# Install+Update   w/   pacman -Syu <pkg>
# Install only     w/   pacman -S <pkg>
# List installed   w/   pacman -Qe
# Uninstall        w/   pacman -Rsc <pgk>
# Search           w/   pacman -Ss <key>
# Refresh mirrors  w/   pacman -Syyu
# Clean cache      w/   pacman -Scc
# Update           w/   pacman -Syu
# Check Orphan pkgs  w/ pacman -Qtdq
# Remove Orphan pkgs w/ sudo pacman -Rns $(pacman -Qtdq)
#
# Update                     w/  yay
# Search statically          w/  yay -Ss
# Upgradable packages        w/  yay -Pu
# Remove unused dependencies w/  yay -Yc
# Delete cache               w/  yay -Scc
#
# Check systemd failed services w/ systemctl --failed
# Clean the cache    w/ rm -rf .cache/*
# Log files check    w/ sudo journalctl -p 3 -xb
# Clean the journal  w/ sudo journalctl --vacuum-time=2weeks


sudo pacman -Syyu
# Uncomment in /etc/pacman.conf
# Color
 
### Install Graphics drivers (choose needed)
# If in doubt just install xorg
# Virtual machines
# sudo pacman -S xf86-video-qxl
# Intel
sudo pacman -S xf86-video-intel
# AMD gpu
# sudo pacman -S xf86-video-amdgpu
# NVIDIA gpu
sudo pacman -S nvidia nvidia-utils


### AUR yay setup
mkdir local
cd local
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si PKGBUILD


### Dotfiles
cd
git clone https://github.com/phdenzel/dotfiles.git
cd dotfiles
. bootstrap.sh
./bootstrap.sh --bin
./bootstrap.sh --emacs


### Display server / manager (choose needed)
# XMonad
sudo pacman -S xorg-server xorg-init xorg-xrandr
sudo pacman -S xmonad xmonad-contrib dmenu
yay -S ly  # lightweight display manager
yay -S brave-bin
yay -S alacritty-git
sudo systemctl enable ly.service
# # LightDM
# > sudo pacman -S lightdm
# > sudo systemctl enable lightdm [lightdm-mini-greeter | lightdm-webkit-greeter | lightdm-webkit2-theme-tty-git]
# > nano /etc/lightdm/lightdm.conf  # Enable themes etc.


# File manager
sudo pacman -S pcmanfm

# Disk utils
sudo pacman -S zip unzip
sudo pacman -S cronie rsync
yay -S timeshift

# Development tools
yay -S alacritty-git
# if no Hardware acceleration is available
# run alacritty with `LIBGL_ALWAYS_SOFTWARE=1 /usr/bin/alacritty`
# and replace Exec in `/usr/share/applications/Alacritty.desktop`
# `Exec=env LIBGL_ALWAYS_SOFTWARE=1 /usr/bin/alacritty`
sudo pacman -S emacs
# sudo pacman -Syu code
sudo pacman -S texlive-most texlive-lang

# Web packages
sudo pacman -S git wget curl lynx

# Virtualization
sudo pacman -S virt-manager qemu qmu-arch-extra edk2-ovmf vde2 bridge-utils
# sudo pacman -S ebtables dnsmasq openbsd-netcat
sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service
# sudo vim br10.xml  # for some distros the default virtual network doesn't work
# sudo virsh net-define br10.xml
# sudo virsh net-start br10
# sudo virsh net-autostart br10

# Fonts
sudo pacman -S \
     ttf-fira-mono ttf-fira-sans \
     ttf-roboto ttf-roboto-mono \
     adobe-source-code-pro-fonts adobe-source-sans-fonts \
     ttf-hack \
     ttf-inconsolata \
     ttf-ubuntu-font-family \
     ttf-font-awesome
yay -S \
    ttf-exo-2 \
    ttf-all-the-icons

# Media packages
yay -S dropbox
dropbox start -i
yay -S enpass-bin brave-bin mailspring
sudo pacman -S feh zathura
sudo pacman -Syu calibre celluloid
curl -sS https://download.spotify.com/debian/pubkey.gpg | gpg --import -
yay -S spotify zenity ffmpeg-compat-57
