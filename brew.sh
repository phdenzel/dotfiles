#!/bin/bash
####################################################### Homebrew installs
# Make sure we’re using the latest Homebrew
brew update
# Upgrade any already-installed formulae
#brew upgrade

# Get latest bash version
# Note: in order to use this build of bash as login shell, it must be added to /etc/shells
brew install bash
# add to /etc/shells and use `chsh` (change shell)
if ! fgrep -q '/usr/local/bin/bash' /etc/shells; then
    echo '/usr/local/bin/bash' | sudo tee -a /etc/shells;
    chsh -s /usr/local/bin/bash;
fi;
# add bash completion
brew install bash-completion

### GNU
# wget with iri support (alternatively use .wgetrc to turn on iri manually)
brew install wget --with-iri
# GNU core utilities - prefixed with g(command)
# to override existing bins add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`
brew install coreutils
# GNU search utilities - prefixed with g(command)
# to override existing bins add `$(brew --prefix findutils)/libexec/gnubin` to `$PATH`
brew install findutils
# more GNU utilities
brew install binutils
brew install moreutils
brew install gnutls
# GNU sed - installed as gsed
brew install gnu-sed
# GNU cal equivalent
brew install gcal

### Useful commands/libs
brew install dtrx
brew install ghostscript
brew install ngrep
brew install gawk
brew install mawk
brew install librsvg
brew install hunspell
# dev commands
brew install pcre
brew install swig
brew install cmake
brew install autoconf
brew install libffi
# ansible - config automation tool
brew install openssl@1.1
brew install ansible

### General frameworks
brew install ocaml
#brew install llvm --with-python
brew install gdb
# c/c++
brew install gcc
brew install sdl2
#brew reinstall gcc --without-multilib
# python
brew install python
brew install python3
# java
# cask tap deprecated in modern versions
#brew tap caskroom/cask
#brew install brew-cask
brew cask install java
brew install ant
brew install jenv
# init jEnv in bashrc with 'if which jenv > /dev/null; then eval "$(jenv init -)"; fi'
# add jdks with 'jenv add /Library/Java/JavaVirtualMachines/jdk1.x.x_xx.jdk/Contents/Home'
# and set a global version with 'jenv global <version>' from 'jenv versions'
# or locally use 'jenv local <version>' in target directory
# Javascript
brew install node
# coffee
npm install -g http-server
npm install -g coffee-script
# ruby
brew install ruby
# LaTeX
brew cask install mactex
# Markdown
brew install pandoc
brew cask install macdown

### Misc
brew install imagemagick --with-webp

### Emacs
# install newest version of emacs
brew install --with-cocoa --with-ctags --with-gnutls --with-imagemagick emacs
brew linkapps emacs

### Some apps
#brew cask install yacreader

### Get rid of the useless stuff
brew cleanup
