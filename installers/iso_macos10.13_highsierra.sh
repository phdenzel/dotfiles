#!/bin/bash

# Download macOS High Sierra first with:
# `open macappstores://itunes.apple.com/app/id1246284741`

hdiutil create -o /tmp/HighSierra.cdr -size 5500m -layout SPUD -fs HFS+J
hdiutil attach /tmp/HighSierra.cdr.dmg -noverify -mountpoint /Volumes/install_build
sudo /Applications/Install\ macOS\ High\ Sierra.app/Contents/Resources/createinstallmedia --volume /Volumes/install_build
mv /tmp/HighSierra.cdr.dmg ~/Desktop/InstallSystem.dmg
hdiutil detach /Volumes/Install\ macOS\ High\ Sierra
hdiutil convert ~/Desktop/InstallSystem.dmg -format UDTO -o ~/Desktop/HighSierra.iso
mv ~/Desktop/HighSierra.iso.cdr ~/Desktop/HighSierra.iso
rm ~/Desktop/InstallSystem.dmg
