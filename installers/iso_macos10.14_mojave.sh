#!/bin/bash

# Download macOS High Sierra first with:
# `open https://itunes.apple.com/ch/app/macos-mojave/id1398502828?ls=1&mt=12&l=de`

hdiutil create -o /tmp/Mojave.cdr -size 8000m -layout SPUD -fs HFS+J
hdiutil attach /tmp/Mojave.cdr.dmg -noverify -mountpoint /Volumes/install_build
sudo /Applications/Install\ macOS\ Mojave.app/Contents/Resources/createinstallmedia --volume /Volumes/install_build
mv /tmp/Mojave.cdr.dmg ~/Desktop/InstallSystem.dmg
hdiutil detach /Volumes/Install\ macOS\ Mojave
hdiutil convert ~/Desktop/InstallSystem.dmg -format UDTO -o ~/Desktop/Mojave.iso
mv ~/Desktop/Mojave.iso.cdr ~/Desktop/Mojave.iso
rm ~/Desktop/InstallSystem.dmg
