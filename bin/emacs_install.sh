#!/bin/bash

cd ~/local/
git clone git://git.savannah.gnu.org/emacs.git
cd emacs/
./autogen.sh
CC=/usr/bin/clang ./configure --with-ns --without-makeinfo
make install
cd nextstep/
cp -r Emacs.app /Applications/
