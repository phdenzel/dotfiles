#!/bin/bash

cd ~/local/
mkdir gcc
cd gcc
wget http://www.netgull.com/gcc/releases/gcc-6.4.0/gcc-6.4.0.tar.gz
tar xzfv gcc-6.4.0.tar.gz
cd gcc-6.4.0
./contrib/download_prerequisites
cd ..
mkdir gcc-build
cd gcc-build
$PWD/../gcc-6.4.0/configure --prefix=$HOME/local/gcc --enable-languages=c,c++,fortran,go

