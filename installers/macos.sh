### Some optimizations
sudo mdutil -i off -a

### Some git sources
mkdir -p $HOME/local
if [ ! -d "$HOME/dotfiles" ]; then
    git clone git@github.com:phdenzel/dotfiles.git
    # cd $HOME/dotfiles
    # ./bootstrap.sh --bin
    # ./bootstrap.sh
    # bash vms/install
done;
# - Generate a new profile using GenSMBIOS.command
# - Test its availability on https://checkcoverage.apple.com/ch/de/
# - Add it to ~/local/OSX-KVM/OpenCore/config[.custom].plist
# - Regenerate ~/local/OSX-KVM/OpenCore/OpenCore.qcow2
#   using the build script ~/local/OSX-KVM/OpenCore/opencore-image-ng.sh
if [ ! -d "$HOME/local/GenSMBIOS" ]; then
    cd $HOME/local
    git clone git@github.com:cropnewt/GenSMBIOS.git
    cd $HOME/local/GenSMBIOS
    chmod +x GenSMBIOS.command
done;
if [ ! -d "$HOME/local/OSX-KVM" ]; then
    cd $HOME/local
    git clone git@github.com:phdenzel/OSX-KVM.git
done;

### Homebrew installs
if [ ! -d "$HOME/local/homebrew-install" ]; then
	git clone git@github.com:Homebrew/install.git ~/local/homebrew-install
	cd ~/local/homebrew-install
	bash install.sh
  cd
fi;

brew update
# Terminal
brew install --cask alacritty
# Browser
brew install --cask brave-browser
# Fonts
brew tap homebrew/cask-fonts
brew install --cask font-fira-code font-fira-mono font-fira-sans
# UNIX pass
brew install pass pinentry-mac
sudo chmod 700 ~/.gnupg
# emacs
brew install --cask emacs

### Some manual install media (homebrew version is broken; tested on 16 Sep 2022)
cd $HOME/Downloads/
curl -L -O https://github.com/osxfuse/osxfuse/releases/download/macfuse-4.4.1/macfuse-4.4.1.dmg
curl -L -O https://github.com/osxfuse/sshfs/releases/download/osxfuse-sshfs-2.5.0/sshfs-2.5.0.pkg
cd

