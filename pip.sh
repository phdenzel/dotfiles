#!/bin/bash

# the essentials
pip install ipython
pip install numpy
pip install matplotlib
pip install scipy
pip install sympy

# general utils
pip install dtrx
pip install --user certifi
export C_INCLUDE_PATH=/usr/local/include/hunspell
# be careful if there's more than a single lib
sudo ln -sf /usr/local/lib/libhunspell-*.a /usr/local/lib/libhunspell.a
pip install hunspell
# for emacs-elpy
pip install jedi
pip install flake8
pip install importmagic
pip install autopep8
pip install yapf
pip install virtualenv
pip install epc

# for more advanced graphics
pip install Pillow

# for the statistics experts
pip install emcee
pip install corner

# for the astrophysicists/astronomers
pip install astropy
pip install wcsaxes
pip install -U scikit-image
