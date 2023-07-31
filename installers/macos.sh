#!/bin/bash
EMAIL="phdenzel@gmail.com"

### Some optimizations
sudo mdutil -i off -a

### Authentication setup
mkdir -p ${HOME}/.gnupg && chmod 700 ${HOME}/.gnupg
mkdir -p ${HOME}/.ssh && chmod 700 ${HOME}/.ssh
if [ ! -f "${HOME}/.ssh/id_ed25519" ]; then
	  ssh-keygen -t ed25519 -q -f "${HOME}/.ssh/id_ed25519" -C "$EMAIL" -N "";
fi;
if [ ! -f "${HOME}/.ssh/gh_id_ed25519" ]; then
	  ssh-keygen -t ed25519 -q -f "${HOME}/.ssh/gh_id_ed25519" -C "$EMAIL" -N "";
	  echo -e "Please register the generated ssh key ${HOME}/.ssh/gh_id_ed25519.pub \n\t$(cat ${HOME}/.ssh/gh_id_ed25519.pub) \n with your GitHub account at https://github.com/settings/keys and re-run this script..."
	  exit 0;
fi;

### dotfiles
if [ ! -d "${HOME}/dotfiles" ]; then
    git clone git@github.com:phdenzel/dotfiles.git
fi;
cd $HOME/dotfiles && ./bootstrap.sh -b -e

### Homebrew install
if [ ! -d "/opt/homebrew" ]; then
	  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)";
	  PATH=/opt/homebrew/bin:$PATH;
fi;

brew update
brew install autoconf bat binutils colordiff coreutils diffutils emacs findutils gawk gnu-sed gnu-tar gnupg grep gzip isync less make mu pass pass-otp pinentry pinentry-mac screen watch wget zip
# casks
brew tap homebrew/cask-fonts
brew install --cask alacritty alfred brave-browser emacs font-fira-code font-fira-mono font-fira-sans
