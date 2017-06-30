#!/bin/bash

# link directories to ~/Documents
echo "Directory links"
for folder in ~/Dropbox/{cheat-sheets,conferences,configs,cv,finances,how-tos,icons,lectures,manuals,master-thesis,papers,personal,presentations,project-stratocaster,research,scores,software,teaching,tolino,travel-docs,wish-cards}; do
    dname=${folder##*/}
    if [ -e "$HOME/Documents/$dname" ]; then
        echo "    Link ~/Documents/$dname does already exist"
    else
        echo "    Linking ~/Dropbox/$dname to ~/Documents/"
        ln -s $folder ~/Documents/$dname
    fi;
done;
unset folder;
unset dname;

# exception when only contents should be linked to ~/Documents
echo "Content links"
for folder in ~/Dropbox/family; do
    dname=${folder##*/}
    if [ -d "$HOME/Documents/$dname" ]; then
        echo "    Directory ~/Documents/$dname already exists"
    else
        echo "    Making a new ~/Documents/$dname directory"
        mkdir $HOME/Documents/$dname
    fi;
    for content in $folder/*; do
        cname=${content##*/}
        if [ -e "$HOME/Documents/$dname/$cname" ]; then
            echo "    Link ~/Documents/$dname/$cname does already exist"
        else
            echo "    Linking ~/Dropbox/$dname/$cname to ~/Documents/$dname/"
            ln -s $HOME/Dropbox/$dname/$cname $HOME/Documents/$dname/$cname
        fi;
    done;
done;
unset folder;
unset content;
unset dname;
unset cname;
