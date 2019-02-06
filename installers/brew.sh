#!/bin/bash
####################################################### Homebrew installs
# Install homebrew
# /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
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

# git
brew install git

### GNU
# wget with iri support (alternatively use .wgetrc to turn on iri manually)
brew install wget
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
#brew install gcal

### General framework
brew install autoconf
brew install automake
brew install cmake
# C/C++
brew install gcc
brew install gcc@6
brew install sdl2
brew install gsl
brew install glpk
brew install eigen
brew install cuba
#brew reinstall gcc --without-multilib
# Python
brew install python
brew install python3
# Java
brew tap caskroom/cask
brew cask install java
brew install ant
brew install jenv
# init jEnv in bashrc with 'if which jenv > /dev/null; then eval "$(jenv init -)"; fi'
# add jdks with 'jenv add /Library/Java/JavaVirtualMachines/jdk1.x.x_xx.jdk/Contents/Home'
# and set a global version with 'jenv global <version>' from 'jenv versions'
# or locally use 'jenv local <version>' in target directory
# Scala
brew install scala
# Web serving
brew install node
# Coffee
#npm install -g http-server
#npm install -g coffee-script
# Ruby
brew install ruby
# Go
brew install go
# Misc
#brew install docker
#brew install boot2docker
brew install ocaml
#brew install valgrind
brew install mono
brew install tesseract --with-all-languages
#brew install llvm --with-python
brew install gdb
# LaTeX
brew cask install mactex
# Markdown
brew install pandoc
brew cask install macdown


### Useful commands/libs
brew install dtrx
brew install ghostscript
brew install ngrep
brew install gawk
brew install mawk
brew install cairo
brew install librsvg
brew install ispell
brew install hunspell
brew install cdiff
brew install xpdf
brew install exiftool
# dev commands
brew install pcre
brew install swig
brew install libffi
# ansible - config automation tool
brew install openssl@1.1
brew install ansible

### Misc
brew install asciinema
brew install ffmpeg
brew install imagemagick --with-webp
brew install imagemagick@6
brew cask install gimp
# inkscape unfortunately glitchy on my machine, which is why I uninstalled it
# brew cask install inkscape
brew install wakeonlan
brew install ical-buddy

### Emacs
# rather compile from the source, it's easier...
# git clone git://git.savannah.gnu.org/emacs.git
# install newest version of emacs
# brew install --with-cocoa --with-ctags --with-gnutls --with-imagemagick emacs
# brew linkapps emacs

### Some apps
brew cask install yacreader

### Get rid of the useless stuff
brew cleanup
