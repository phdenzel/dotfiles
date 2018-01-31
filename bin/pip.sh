#!/bin/bash

PIP='pip'
# PIP='pip3'

# the essentials
$PIP install numpy
$PIP install matplotlib
$PIP install scipy
$PIP install sympy
$PIP install pandas
$PIP install virtualenv
$PIP install sphinx
$PIP install ipython
$PIP install jupyter

# general utils
$PIP install dtrx
$PIP install pyyaml
$PIP install requests
$PIP install --user certifi
# export C_INCLUDE_PATH=/usr/local/include/hunspell
# be careful if there's more than a single lib
# sudo ln -sf /usr/local/lib/libhunspell-*.a /usr/local/lib/libhunspell.a
# $PIP install hunspell
$PIP install line_profiler
$PIP install memory_profiler
# for emacs-elpy
$PIP install jedi
$PIP install flake8
$PIP install importmagic
$PIP install autopep8
$PIP install yapf
$PIP install epc

# for web servings
$PIP install flask

# for more advanced graphics
$PIP install Pillow
$PIP install opencv-python

# Google's TensorFlow and other machine learning libs
$PIP install tensorflow
$PIP install keras

# for the statistics experts
$PIP install emcee
$PIP install corner

# for the Arduino user
$PIP install pyserial

# for tinkering
$PIP install pyowm
$PIP install wikipedia
$PIP install wolframalpha

# for the astrophysicists/astronomers
$PIP install astropy
# unknown error
# $PIP install wcsaxes
$PIP install -U scikit-image
