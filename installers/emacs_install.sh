#!/bin/bash
mkdir -p $HOME/local
cd $HOME/local
git clone https://git.savannah.gnu.org/git/texinfo.git
git clone https://git.savannah.gnu.org/emacs.git
cd emacs/
./autogen.sh
if [ $OS == "Darwin" ]; then
    CC=/usr/bin/clang ./configure --with-ns --without-makeinfo
    make && make install
    cd nextstep/
    cp -r Emacs.app /Applications/
elif [ $OS == "Windows" ]; then
    echo "In Windows better use the .exe installer"
else
    echo "In linux use your package manager, such as apt, pacman, zypper, etc."
fi;
