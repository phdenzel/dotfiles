####################################################### bashrc
if `uname -a | grep -q "Linux"`; then
    source ~/.bash_profile;
elif [ -n "$OS" ]; then
    source ~/.bash_profile;
fi;

