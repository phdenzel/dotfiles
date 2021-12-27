### Arch setup
# Install+Update   w/   pacman -Syu <pkg>
# Install only     w/   pacman -S <pkg>
# List installed   w/   pacman -Qe
# Uninstall        w/   pacman -Rsc <pgk>
# Search           w/   pacman -Ss <key>
# Refresh mirrors  w/   pacman -Syyu
# Update           w/   pacman -Syu
#
# Upgradable packages        w/   yay -Pu
# Remove unused dependencies w/   yay -Yc

sudo pacman -Syyu

# AUR setup
mkdir local
cd local
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si PKGBUILD

# Disk utils
sudo pacman -Syu cron
yay -S timeshift

# Development tools
yay -S alacritty-git
# if no Hardware acceleration is available
# run alacritty with `LIBGL_ALWAYS_SOFTWARE=1 /usr/bin/alacritty`
# and replace Exec in `/usr/share/applications/Alacritty.desktop`
# `Exec=env LIBGL_ALWAYS_SOFTWARE=1 /usr/bin/alacritty`
sudo pacman -Syu emacs
# sudo pacman -Syu code
sudo pacman -Syu texlive-most texlive-lang

# Web packages
sudo pacman -Syu wget git curl lynx

# Fonts
sudo pacman -Syu ttf-fira-mono ttf-fira-code ttf-fira-mono ttf-fira-sans ttf-roboto ttf-roboto-mono ttf-font-awesome tff-all-the-icons
yay -S ttf-roboto-slab

# GNOME Customization
# sudo pacman -Syu gnome-tweaks
# sudo pacman -S chrome-gnome-shell
## Export GNOME key-shortcuts
#dconf dump / | sed -n '/\[org.gnome.settings-daemon.plugins.media-keys/,/^$/p' > custom-shortcuts$(date -I).ini
## Import GNOME key-shortcuts
#dconf load / < custom-shortcuts.ini

# Media packages
yay -S enpass-bin dropbox
dropbox start -i
yay -S brave-bin mailspring
sudo pacman -Syu calibre celluloid
curl -sS https://download.spotify.com/debian/pubkey.gpg | gpg --import -
yay -S spotify zenity ffmpeg-compat-57
