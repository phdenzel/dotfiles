### Pop!_OS setup
sudo apt update

# Web
sudo apt install wget git curl apt-transport-https
sudo apt install lynx
sudo apt install network-manager-openconnect network-manager-openconnect-gnome
sudo apt install transmission

# Disk utils
sudo apt install gparted timeshift

# Desktop utils
sudo apt install htop
sudo apt install gnome-tweaks
sudo apt install gnome-shell-extension-gsconnect
sudo apt install gnome-shell-extension-system-monitor
sudo apt install lm-sensors sensors-applet
sudo sensors-detect
sudo apt install hddtemp nvme-cli
sudo apt install psensor

# Virtualization
sudo apt install qemu-kvm libvirt-daemon-system libvirt-clients bridge-utils
sudo apt install virt-manager virt-viewer

# Text streaming
sudo apt install colordiff
sudo apt install gawk

# LaTeX
sudo apt install texlive-full

# Fonts
sudo apt install gsfonts-x11
sudo apt install fonts-hack

# Audio / Video encoding
sudo apt install sox
sudo apt install ffmpeg
sudo apt install libcairo2 libcairo2-dev

# Programming
sudo apt install pipenv
sudo apt install ruby-full

# Development utils
sudo apt install alacritty
sudo apt install emacs-gtk
sudo apt install build-essential cmake
sudo apt install swig
sudo apt install npm
sudo apt install python2-dev
sudo apt install python-tk
sudo apt install python3-opencv
sudo apt install python3-sklearn
sudo apt install pylint

# Other devs
sudo apt install libavcodec-dev libavformat-dev libswscale-dev
sudo apt install qtdeclarative5-dev
sudo apt install libxinerama-dev libxrandr-dev libxcursor-dev libxi-dev libx11-dev libxtst-dev

# Media and Gaming
sudo apt install calibre
sudo pat install spotify-client
sudo apt install celluloid
sudo apt install dxvk
sudo dpkg --add-architecture i386
wget -nc https://dl.winehq.org/wine-builds/winehq.key
sudo apt-key add winehq.key
sudo add-apt-repository 'deb https://dl.winehq.org/wine-builds/ubuntu/ focal main'
sudo apt update
sudo apt install --install-recommends winehq-staging
sudo apt install winbind winetricks
winetricks corefonts
winetricks vcrun2015
sudo apt install steam
sudo apt install lutris
# Some Lutris dependencies
sudo apt install libgnutls30:i386 libldap-2.4-2:i386 libgpg-error0:i386 libsqlite3-0:i386

# Device support software
sudo apt install piper

### External debs
# Corsair keyboard driver
sudo apt install libudev-dev qt5-default zlib1g-dev libpulse-dev libquazip5-dev libqt5x11extras5-dev libxcb-screensaver0-dev libxcb-ewmh-dev libxcb1-dev qttools5-dev-tools libdbusmenu-qt5-dev
mkdir ~/forks
git clone https://github.com/phdenzel/ckb-next.git ~/forks/ckb-next
cd ~/forks/ckb-next
git checkout 1555ca9  # release v0.4.2
./quickinstall
cd

# Dropbox
echo "deb https://linux.dropbox.com/ubuntu bionic main" | sudo tee /etc/apt/sources.list.d/dropbox.list
# sudo apt-key adv --keyserver pgp.mit.edu --recv-keys 1C61A2656FB57B7E4DE0F4C1FC918B335044912E
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 1C61A2656FB57B7E4DE0F4C1FC918B335044912E
sudo apt update
sudo apt install python3-gpg dropbox
nautilus --quit

# Enpass
echo "deb https://apt.enpass.io/ stable main" | sudo tee /etc/apt/sources.list.d/enpass.list
wget -O - https://apt.enpass.io/keys/enpass-linux.key | sudo apt-key add -
sudo apt update
sudo apt install enpass

# Brave
curl -s https://brave-browser-apt-release.s3.brave.com/brave-core.asc | sudo apt-key --keyring /etc/apt/trusted.gpg.d/brave-browser-release.gpg add -
echo "deb [arch=amd64] https://brave-browser-apt-release.s3.brave.com/ stable main" | sudo tee /etc/apt/sources.list.d/brave-browser-release.list
sudo apt update
sudo apt install brave-browser

# Mailspring
wget -O ~/Downloads/mailspring.deb "https://updates.getmailspring.com/download?platform=linuxDeb"
sudo apt install ~/Downloads/mailspring.deb

# VS Code
wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
sudo install -o root -g root -m 644 packages.microsoft.gpg /etc/apt/trusted.gpg.d/
sudo sh -c 'echo "deb [arch=amd64 signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'
sudo apt update
sudo apt install code

# MEGA sync Desktop app
curl https://mega.nz/linux/MEGAsync/xUbuntu_20.04/amd64/megasync-xUbuntu_20.04_amd64.deb --output ~/Downloads/megasync-xUbuntu_20.04_amd64.deb
sudo apt install ~/Downloads/megasync-xUbuntu_20.04_amd64.deb
