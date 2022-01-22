#!/bin/bash

# Download macOS Monterey first with:
# `open https://apps.apple.com/us/app/macos-monterey/id1576738294`

hdiutil create -o /tmp/Monterey.cdr -size 16500m -volume Monterey -layout SPUD -fs HFS+J
hdiutil attach /tmp/Monterey.cdr.dmg -noverify -mountpoint /Volumes/Monterey
sudo /Applications/Install\ macOS\ Monterey.app/Contents/Resources/createinstallmedia --volume /Volumes/Monterey
mv /tmp/Monterey.cdr.dmg ~/Desktop/Monterey.dmg
hdiutil detach /Volumes/Install\ macOS\ Monterey
hdiutil convert ~/Desktop/Monterey.dmg -format UDTO -o ~/Desktop/Monterey.iso
mv ~/Desktop/Monterey.iso.cdr ~/Desktop/Monterey.iso
rm ~/Desktop/Monterey.dmg
