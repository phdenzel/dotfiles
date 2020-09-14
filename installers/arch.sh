### Arch setup
# Install+Update   w/   pacman -Syu <pkg>
# Install only     w/   pacman -S <pkg>
# Uninstall        w/   pacman -Rsc <pgk>
# Search           w/   pacman -Ss <key>
# Refresh mirrors  w/   pacman -Syyu
# Update           w/   pacman -Syu

sudo pacman -Syyu

# AUR setup
mkdir local
cd local
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si PKGBUILD

# Development tools
sudo pacman -Syu alacritty
sudo pacman -Syu emacs

# GNOME Customization
sudo pacman -Syu gnome-tweaks
sudo pacman -Syu ttf-fira-mono community/tff-fira-code community/tff-fira-mono community/tff-fira-sans community/tff-roboto community/tff-roboto-mono
yay -S tff-roboto-slab
## Export GNOME key-shortcuts
#dconf dump / | sed -n '/\[org.gnome.settings-daemon.plugins.media-keys/,/^$/p' > custom-shortcuts$(date -I).ini
## Import GNOME key-shortcuts
#dconf load / < custom-shortcuts.ini


# Disk utils
sudo pacman -Syu cron
yay -S timeshift

# Web packages
sudo pacman -Syu wget git curl
sudo pacman -Syu lynx
yay -S brave-bin
yay -S mailspring
