#!/bin/bash

# the essentials
pip install ipython
pip install numpy
pip install matplotlib
pip install scipy
pip install sympy
pip install virtualenv

# general utils
pip install dtrx
pip install requests
pip install --user certifi
export C_INCLUDE_PATH=/usr/local/include/hunspell
# be careful if there's more than a single lib
sudo ln -sf /usr/local/lib/libhunspell-*.a /usr/local/lib/libhunspell.a
pip install hunspell
pip install line_profiler
pip install memory_profiler
# for emacs-elpy
pip install jedi
pip install flake8
pip install importmagic
pip install autopep8
pip install yapf
pip install epc

# for web servings
pip install flask

# for more advanced graphics
pip install Pillow
pip install opencv-python

# Google's TensorFlow and other machine learning libs
pip install tensorflow
pip install keras

# for the statistics experts
pip install emcee
pip install corner

# for the Arduino user
pip install pyserial

# for tinkering
pip install pyowm
pip install wikipedia
pip install wolframalpha

# for the astrophysicists/astronomers
pip install astropy
pip install wcsaxes
pip install -U scikit-image
