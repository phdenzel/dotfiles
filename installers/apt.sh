#!/bin/bash
####################################################### Homebrew installs
# Remove non-essentials
sudo apt-get remove --purge gnome-games
sudo apt-get autoremove
# Install apt packages
# Essentials
sudo apt-get install gnome-core        # GNOME desktop manager
sudo apt-get install gnome-calculator  # for gtk
sudo apt-get install git               # very likely already installed
sudo apt-get install ssh
sudo apt-get install wget
sudo apt-get install x11-apps
# System utilities
sudo apt-get install sysstat
# Dev utilities
sudo apt-get install autoconf
sudo apt-get install gcc
sudo apt-get install build-essential
sudo apt-get install checkinstall
sudo apt-get install gnutls-bin
sudo apt-get install python python3
sudo apt-get install ruby ruby-dev
# Libs
sudo apt-get install libgnutls28-dev
sudo apt-get install libx11-dev
sudo apt-get install libjpeg-dev
sudo apt-get install libgif-dev
sudo apt-get install libtiff5-dev
sudo apt-get install libncurses5-dev
sudo apt-get install libxft-dev
sudo apt-get install librsvg2-dev
sudo apt-get install libmagickcore-dev
sudo apt-get install libmagick++-dev
sudo apt-get install libxml2-dev
sudo apt-get install libgpm-dev
sudo apt-get install libotf-dev
sudo apt-get install libm17n-dev
sudo apt-get install libgtk-3-dev
sudo apt-get install libwebkitgtk-3.0-dev
sudo apt-get install libxpm-dev
sudo apt-get install libxaw7-dev

# Misc
sudo apt-get install imagemagick
sudo apt-get install emacs
