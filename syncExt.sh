#!/bin/bash

# external drives
EXTD=LaCieSHARE

# settings
FLAGS=ahP

DO_DOCS=1
DO_HOME=1
DO_MOVS=0
DO_MUSC=1
DO_PICS=1
DO_DBOX=0

# look for places external drives are mounted
if [ -d "/Volumes" ]; then
    MNT=/Volumes
elif [ -d "/mnt" ]; then
    MNT=/mnt
else
    echo "/Volumes not found..."
    exit 1
fi;
echo "Looking in $MNT for external drives..."

# look for the external drives
if [ -d $MNT/$EXTD ]; then
    echo "Detected external drive: $MNT/$EXTD"
    ls $MNT/$EXTD
else
    echo "External drive $EXTD not found..."
    exit 1
fi;

# synch ~/Documents
printf '#%.0s' {1..79}
echo
echo "DOCUMENTS sync"
echo
if [ -d "$MNT/$EXTD/Documents" ] && [ $DO_DOCS -eq 1 ]; then
    rsync --exclude ".DS_Store" \
          --exclude ".localized" \
          --exclude "phd.terminal" \
          --exclude "syncExtD.sh" \
          --exclude "workspace/" \
          -$FLAGS -L "$HOME/Documents/" "$MNT/$EXTD/Documents/";
else
    echo "Skipping $MNT/$EXTD/Documents..."
fi;

# synch ~/
printf '#%.0s' {1..79}
echo
echo "HOME sync"
echo
if [ -d "$MNT/$EXTD/Home" ] && [ $DO_HOME -eq 1 ]; then
    rsync --exclude ".*" \
          --exclude "matplotlibrc" \
          --exclude "Applications/" \
          --exclude "Desktop/" \
          --exclude "Documents/" \
          --exclude "Downloads/" \
          --exclude "Dropbox/" \
          --exclude "Google\ Drive/" \
          --exclude "Library/" \
          --exclude "Movies/" \
          --exclude "Music/" \
          --exclude "Pictures/" \
          --exclude "Public/" \
          -$FLAGS "$HOME/" "$MNT/$EXTD/Home/";
else
    echo "Skipping $MNT/$EXTD/Home..."
fi;

# synch ~/Movies
printf '#%.0s' {1..79}
echo
echo "MOVIES sync"
echo
if [ -d "$MNT/$EXTD/Movies" ] && [ $DO_MOVS -eq 1 ]; then
    rsync --exclude ".DS_Store" \
          --exclude ".localized" \
          -$FLAGS "$HOME/Movies/" "$MNT/$EXTD/Movies/";
else
    echo "Skipping $MNT/$EXTD/Movies..."
fi;

# synch ~/Music
printf '#%.0s' {1..79}
echo
echo "MUSIC sync"
echo
if [ -d "$MNT/$EXTD/Music" ] && [ $DO_MUSC -eq 1 ]; then
    rsync --exclude ".DS_Store" \
          --exclude ".localized" \
          -$FLAGS "$HOME/Music/" "$MNT/$EXTD/Music/";
else
    echo "Skipping $MNT/$EXTD/Music..."
fi;

# synch ~/Pictures
printf '#%.0s' {1..79}
echo
echo "PICTURES sync"
echo
if [ -d "$MNT/$EXTD/Pictures" ] && [ $DO_PICS -eq 1 ]; then
    rsync --exclude ".DS_Store" \
          --exclude ".localized" \
          --exclude "Google\ Photos\ Backup/" \
          -$FLAGS "$HOME/Pictures/" "$MNT/$EXTD/Pictures/";
else
    echo "Skipping $MNT/$EXTD/Pictures..."
fi;

# synch ~/Dropbox
printf '#%.0s' {1..79}
echo
echo "DROPBOX sync"
echo
if [ -d "$MNT/$EXTD/Dropbox" ] && [ $DO_DBOX -eq 1 ]; then
    rsync --exclude ".DS_Store" \
          --exclude ".dropbox" \
          --exclude ".dropbox.cache/" \
          --exclude "Icon\015" \
          -$FLAGS "$HOME/Dropbox/" "$MNT/$EXTD/Dropbox/";
else
    echo "Skipping $MNT/$EXTD/Dropbox..."
fi;
