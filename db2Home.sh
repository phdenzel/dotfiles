#!/bin/bash

# link directories to ~/
echo "Directory links"
for folder in ~/Dropbox/{papers,teaching}; do
    dname=${folder##*/}
    if [ -e "$HOME/$dname" ]; then
        echo "    Link ~/$dname does already exist"
    else
        echo "    Linking ~Dropbox/$dname to ~/"
        ln -s $folder ~/$dname
    fi;
done;
unset folder;
unset dname;

# exception when only contents should be linked to ~/
echo "Content links"
for folder in ~/Dropbox/lensing; do
    dname=${folder##*/}
    # if [ -d "$HOME/$dname" ]; then
    #     echo "Directory ~/$dname already exists"
    # else
    #     echo "Making a new ~/$dname directory"
    #     mkdir $HOME/$dname
    # fi;
    for content in $folder/*; do
        cname=${content##*/}
        if [ -e "$HOME/$cname" ]; then
            echo "    Link ~/$cname does already exist"
        else
            echo "    Linking ~Dropbox/$dname/$cname to ~/"
            ln -s $HOME/Dropbox/$dname/$cname $HOME/$cname
        fi;
    done;
done;
unset folder;
unset content;
unset dname;
unset cname;
