#!/bin/bash

cd ~/local/
git clone git://git.savannah.gnu.org/emacs.git
cd emacs/
./autogen.sh
if [ $OS == "Darwin" ]; then
    CC=/usr/bin/clang ./configure --with-ns --without-makeinfo
    make && make install
    cd nextstep/
    cp -r Emacs.app /Applications/
elif [ $OS == "Linux" ]; then
    ./configure --prefix=$HOME/local/emacs --with-gif=no --with-gnutls=no
    make && make install
    ln -s $HOME/local/emacs/bin/emacs $HOME/local/bin/emacs
elif [ $OS == "Windows" ]; then
    echo "In Windows better use the .exe installer"
fi;
