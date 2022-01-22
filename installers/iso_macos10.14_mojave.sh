#!/bin/bash

# Download macOS Mojave first with:
# `open https://itunes.apple.com/ch/app/macos-mojave/id1398502828`

hdiutil create -o /tmp/Mojave.cdr -size 8000m -volname Mojave -layout SPUD -fs HFS+J
hdiutil attach /tmp/Mojave.cdr.dmg -noverify -mountpoint /Volumes/Mojave
sudo /Applications/Install\ macOS\ Mojave.app/Contents/Resources/createinstallmedia --volume /Volumes/Mojave
mv /tmp/Mojave.cdr.dmg ~/Desktop/Mojave.dmg
hdiutil detach /Volumes/Install\ macOS\ Mojave
hdiutil convert ~/Desktop/Mojave.dmg -format UDTO -o ~/Desktop/Mojave.iso
mv ~/Desktop/Mojave.iso.cdr ~/Desktop/Mojave.iso
rm ~/Desktop/Mojave.dmg
