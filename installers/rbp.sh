### Raspberry Pi setup
sudo apt update
sudo apt upgrade


# Web
sudo apt install wget git curl


# Desktop utils
sudo apt install htop
sudo apt install openssh-server
sudo apt install realvnc-vnc-server realvnc-vnc-viewer


# Text streaming
sudo apt install colordiff
sudo apt install gawk
sudo apt install qrencode


# LaTeX
sudo apt install texlive-full


# Audio / Video encoding
sudo apt install ffmpeg


# Programming
sudo apt install pipenv


# Development utils
sudo apt install alacritty
sudo apt install emacs


# Dropbox
echo "deb https://linux.dropbox.com/ubuntu bionic main" | sudo tee /etc/apt/sources.list.d/dropbox.list
# sudo apt-key adv --keyserver pgp.mit.edu --recv-keys 1C61A2656FB57B7E4DE0F4C1FC918B335044912E
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 1C61A2656FB57B7E4DE0F4C1FC918B335044912E
sudo apt update
sudo apt install python3-gpg dropbox


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
# if gvfs-bin error:
# use `sudo dpkg --ignore-depends=gvfs-bin -i mailspring.deb`
# and erase dependency in mailspring entry of `/var/lib/dpkg/status`


# VS Code
wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
sudo install -o root -g root -m 644 packages.microsoft.gpg /etc/apt/trusted.gpg.d/
sudo sh -c 'echo "deb [arch=amd64 signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'
sudo apt update
sudo apt install code


# MEGA sync Desktop app
curl https://mega.nz/linux/MEGAsync/xUbuntu_20.04/amd64/megasync-xUbuntu_20.04_amd64.deb --output ~/Downloads/megasync-xUbuntu_20.04_amd64.deb
sudo apt install ~/Downloads/megasync-xUbuntu_20.04_amd64.deb
