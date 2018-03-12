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
# OS is set in .bash_profile
if [ $OS == "Linux" ]; then
	../gcc-6.4.0/configure -v --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu --prefix=$HOME/local/gcc-6 --enable-checking=release --enable-languages=c,c++,fortran --disable-multilib --program-suffix=-6
else
	../gcc-6.4.0/configure --prefix=$HOME/local/gcc --enable-languages=c,c++,fortran,go
fi;
