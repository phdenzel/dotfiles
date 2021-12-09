### Raspian setup and configuration
# Setup Wifi: /etc/wpa_supplicant/wpa_supplicant.conf
sudo mv /etc/wpa_supplicant/wpa_supplicant.conf /etc/wpa_supplicant/wpa_supplicant.conf.bak
sudo awk '/DenzelsWLAN/ { print; print "\tscan_ssid=1"; next }1' /etc/wpa_supplicant/wpa_supplicant.conf.bak | sudo tee /etc/wpa_supplicant/wpa_supplicant.conf

# Keyboard CapsLock -> Ctrl
sudo sed -i -e 's/XKBOPTIONS=""/XKBOPTIONS="ctrl:nocaps"/g' /etc/default/keyboard
#sudo dpkg-reconfigure keyboard-configuration
sudo reboot

# Check if Wifi is set up correctly, then update
sudo apt update
sudo apt upgrade
sudo apt autoremove

sudo apt install apt-transport-https wget curl git zip

sudo apt install openssh-server realvnc-vnc-server realvnc-vnc-viewer

sudo apt install ncal
sudo perl -pi -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/g' /etc/locale.gen
sudo locale-gen en_US.UTF-8
sudo update-locale en_US.UTF-8

sudo apt install emacs code
sudo apt remove thonny geany
sudo apt autoremove

# Dotfiles
git clone https://github.com/phdenzel/dotfiles.git
cd $HOME/dotfiles
./bootstrap.sh --emacs
. bootstrap.sh

# SSH setup
mkdir -p $HOME/local/bin
ssh-keygen -t rsa -b 4096 -C "phdenzel@gmail.com"
# Add ~/.ssh/id_rsa.pub to GitHub

# Themes
sudo apt install software-properties-common
sudo apt install sassc meson libglib2.0-dev

cd $HOME/local
git clone https://github.com/pop-os/gtk-theme.git pop-gtk-theme
cd pop-gtk-theme
meson build && cd build
ninja
ninja install

cd $HOME/local
git clone https://github.com/pop-os/icon-theme pop-icon-theme
cd pop-icon-theme
meson build
sudo ninja -C "build" install

mkdir $HOME/.themes
cd $HOME/local
git clone git@github.com:phdenzel/openbox-theme-equilux-compact.git
cd openbox-theme-equilux-compact
unzip equilux-comp.zip
cp -r Equilux-compact ~/.themes/
cp -r Equilux-compact.obt ~/.themes/

# Fonts
cd $HOME/Downloads
mkdir $HOME/.fonts
curl https://fonts.google.com/download?family=Fira%20Mono --output Fira_Mono.zip
unzip Fira_Mono.zip
cp *.ttf $HOME/.fonts/
git clone https://github.com/domtronn/all-the-icons.el.git
cp all-the-icons.el/fonts/*.ttf $HOME/.fonts/
mv all-the-icons.el $HOME/local/

fc-cache -v -f

# Neofetch
mkdir $HOME/forks
cd $HOME/forks
git clone git@github.com:phdenzel/neofetch.git
cd neofetch
make install

# Brave browser
# sudo curl -fsSLo /usr/share/keyrings/brave-browser-archive-keyring.gpg https://brave-browser-apt-release.s3.brave.com/brave-browser-archive-keyring.gpg
# echo "deb [signed-by=/usr/share/keyrings/brave-browser-archive-keyring.gpg arch=amd64] https://brave-browser-apt-release.s3.brave.com/ stable main" | sudo tee /etc/apt/sources.list.d/brave-browser-release.list
# sudo apt update

# sudo apt install build-essential python2.7 python-setuptools
# sudo apt install nodejs npm

# git clone git@github.com:brave/brave-browser.git
