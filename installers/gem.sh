#!/bin/bash
####################################################### Ruby gem installs
# Update first before installing gems
sudo gem update
# if not working (due to permissions error or something)
# use 'brew install ruby' first

# Install ruby gems
gem install jekyll
gem install redcarpet
gem install haml
gem install iStats
