### Arch setup 
# For an all-in-one command, see bottom
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
# Uncomment in /etc/pacman.conf '#Color'
sudo sed -i 's/#Color/Color/g' /etc/pacman.conf


### Install Graphics drivers (choose needed)
# If in doubt just install a bunch (sudo pacman -Ss xf86-video)
# Virtual machines
# sudo pacman -S xf86-video-qxl
# Intel
sudo pacman -S xf86-video-intel
# AMD gpu
# sudo pacman -S xf86-video-amdgpu
# NVIDIA gpu
sudo pacman -S nvidia nvidia-utils


# Generate an SSH key (and copy the public key to all trusted hosts)
sudo pacman -S openssh
ssh-keygen -t ed25519 -C "phdenzel@gmail.com"


### AUR yay setup
mkdir ~/local
cd local
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si PKGBUILD


### My own dotfiles (xmonad configs)
cd
# git clone https://github.com/phdenzel/dotfiles.git
git clone git@github.com:phdenzel/dotfiles.git
cd dotfiles
./bootstrap.sh --bin
./bootstrap.sh --emacs
. bootstrap.sh

./bootstrap.sh --gtk
gsettings set org.gnome.desktop.interface gtk-theme phd-dark
# for applications requiring sudo rights, sudo copy
# .themes/phd-dark to /usr/share/themes/ and
# .config/gtk-3.0/settings.ini to /usr/share/gtk-3.0/

# if this doesn't work use lxappearance
# > sudo pacman -S lxappearance

cd ~/local
git clone git@github.com:phdenzel/neofetch.git
cd neofetch
make install


### Display server setup
# XMonad
sudo pacman -S xorg-server xorg-apps xorg-xinit xorg-xmessage xorg-xrandr \
     libx11 libxft libxinerama libxrandr libxss pkgconf stack wireless_tools
mkdir -p ~/local/xmonad
cd ~/local/xmonad
stack setup
stack upgrade
git clone https://github.com/xmonad/xmonad.git
git clone https://github.com/xmonad/xmonad-contrib.git
git clone https://github.com/jaor/xmobar.git
stack init
cp ~/dotfiles/.config/xmonad/stack.yml stack.yml
stack install
sudo ln -s ~/.local/bin/xmonad /usr/bin
sudo mkdir -p /usr/share/xsessions
sudo ln -s ~/.config/xmonad/xmonad.desktop /usr/share/xsessions


# Window compositor and image viewer (for setting wallpapers)
sudo pacman -S picom feh
# XMonad (install xterm if you don't want to use my xmonad config)
# sudo pacman -S xmonad xmonad-contrib dmenu #xterm
# Display manager  (skip if you want to manually execute xstart)
yay -S ly  # most lightweight DM I know
sudo systemctl enable ly.service
# or use LightDM (still lightweight, but just a bit less)
# > sudo pacman -S lightdm lightdm-webkit2-greeter
# > sudo systemctl enable lightdm [lightdm-mini-greeter | lightdm-webkit-greeter | lightdm-webkit2-theme-tty-git]
# > vim /etc/lightdm/lightdm.conf  # Enable themes etc.
# Browser, terminal, and other programs useful for further setup
yay -S brave-bin alacritty-git 
sudo pacman -S python-pip python-pipenv  # Python (should already be installed, but pip seems not to be)
cd local
git clone git@github.com:phdenzel/xmobar_wttr.git
cd xmobar_wttr
make pkg
python3 setup.py install --user
# If you want a desktop environment instead use
# > sudo pacman -S gdm gnome
# > sudo systemctl enable gdm


# File manager
sudo pacman -S pcmanfm ranger


# System tools
sudo pacman -S xscreensaver

# Disk utils
sudo pacman -S zip unzip rsync cronie htop
# Snapshot/Backup utility
yay -S timeshift


# Development tools
# yay -S alacritty-git
# if no Hardware acceleration is available
# run alacritty with `LIBGL_ALWAYS_SOFTWARE=1 /usr/bin/alacritty`
# and replace Exec in `/usr/share/applications/Alacritty.desktop`
# `Exec=env LIBGL_ALWAYS_SOFTWARE=1 /usr/bin/alacritty`
# My favorite editor(s) (vim usually already installed during grub-install)
sudo pacman -S emacs code vim
# LaTeX
sudo pacman -S texlive-most texlive-lang
# Haskell
sudo pacman -S ghc-static

# Web packages (lynx: blazingly fast, text-based browser)
sudo pacman -S git wget curl lynx
yay -S brave-bin     # privacy-oriented browser

# Utility tools
sudo pacman -S colordiff qalculate-gtk xclip xsel scrot 

# Password manager
sudo pacman -S pass pass-otp
# pass-import (for Enpass import)
yay -S pass-import
# or if you don't trust AUR
cd forks
# git clone https://github.com/phdenzel/pass-import.git
git clone git@github.com:phdenzel/pass-import.git
cd pass-import
python3 setup.py install --user


# Virtualization (see arch_vm.sh for VM init)
sudo pacman -S virt-manager qemu qemu-arch-extra edk2-ovmf vde2 bridge-utils
# sudo pacman -S ebtables dnsmasq openbsd-netcat
sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service
# sudo vim br10.xml  # for some distros the default virtual network doesn't work
# sudo virsh net-define br10.xml
# sudo virsh net-start br10
# sudo virsh net-autostart br10


# Fonts
sudo pacman -S ttf-dejavu ttf-fira-mono ttf-fira-sans ttf-roboto \
     ttf-roboto-mono adobe-source-code-pro-fonts adobe-source-sans-fonts \
     ttf-hack ttf-inconsolata ttf-ubuntu-font-family ttf-font-awesome
yay -S otf-nerd-fonts-fira-mono ttf-exo-2 ttf-all-the-icons ttf-weather-icons
# for more fonts go to https://www.nerdfonts.com/font-downloads
# Make your own psf fonts
# > yay -S otf2bdf bdf2psf
# > ~/local/bin/psf_from_ttf DejaVuSansMono 16 96
# > mkdir -p ~/local/fonts
# > mv DejaVuSansMono.psf ~/.fonts
# Set tty font
echo "FONT=DejaVuSansMono.psf" | sudo tee /etc/vconsole.conf
sudo cp .fonts/*.psf /usr/share/kbd/consolefonts/
fc-cache -v -f
sudo sed -i 's/keyboard/consolefont keyboard/' /etc/mkinitcpio.conf
# Add consolefont to the /etc/mkinitcpio.conf HOOKS
sudo mkinitcpio -p linux


# Media packages
sudo pacman -S zathura calibre celluloid
yay -S dropbox enpass-bin brave-bin mailspring spotify zenity ffmpeg-compat-57
dropbox start -i
curl -sS https://download.spotify.com/debian/pubkey.gpg | gpg --import -


# All in one install
sudo pacman -Syyu
sudo pacman -Syu
sudo pacman -S xf86-video-intel nvidia nvidia-utils openssh xorg-server xorg-apps xorg-xinit xorg-xmessage xorg-xrandr libx11 libxft libxinerama libxrandr libxss pkgconf stack wireless_tools picom feh dmenu python-pip python-pipenv pcmanfm ranger xscreensaver zip unzip rsync cronie htop emacs texlive-most texlive-lang ghc-static git wget curl lynx colordiff qalculate-gtk xclip xsel scrot pass pass-otp virt-manager qemu qemu-arch-extra edk2-ovmf vde2 bridge-utils ttf-dejavu ttf-fira-mono ttf-fira-sans ttf-roboto ttf-roboto-mono adobe-source-code-pro-fonts adobe-source-sans-fonts ttf-hack ttf-inconsolata ttf-ubuntu-font-family ttf-font-awesome zathura calibre mpv celluloid
