#!/bin/bash
LOCALINST=$HOME/local/neofetch

cd $HOME
git clone git@github.com:phdenzel/neofetch.git
cd neofetch

mkdir -p $LOCALINST
make PREFIX=$$LOCALINST install

ln -s $HOME/local/neofetch/bin/neofetch $HOME/local/bin/neofetch

