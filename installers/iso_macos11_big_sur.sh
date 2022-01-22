#!/bin/bash

# Download macOS BigSur first with:
# `open https://apps.apple.com/qa/app/macos-big-sur/id1526878132`

hdiutil create -o /tmp/BigSur.cdr -size 16500m -volume BigSur -layout SPUD -fs HFS+J
hdiutil attach /tmp/BigSur.cdr.dmg -noverify -mountpoint /Volumes/BigSur
sudo /Applications/Install\ macOS\ BigSur.app/Contents/Resources/createinstallmedia --volume /Volumes/BigSur
mv /tmp/BigSur.cdr.dmg ~/Desktop/BigSur.dmg
hdiutil detach /Volumes/Install\ macOS\ BigSur
hdiutil convert ~/Desktop/BigSur.dmg -format UDTO -o ~/Desktop/BigSur.iso
mv ~/Desktop/BigSur.iso.cdr ~/Desktop/BigSur.iso
rm ~/Desktop/BigSur.dmg
