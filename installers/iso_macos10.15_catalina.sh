#!/bin/bash

# Download macOS Catalina first with:
# `open https://apps.apple.com/us/app/macos-catalina/id1466841314`

hdiutil create -o /tmp/Catalina.cdr -size 8500m -volume Catalina -layout SPUD -fs HFS+J
hdiutil attach /tmp/Catalina.cdr.dmg -noverify -mountpoint /Volumes/Catalina
sudo /Applications/Install\ macOS\ Catalina.app/Contents/Resources/createinstallmedia --volume /Volumes/Catalina
mv /tmp/Catalina.cdr.dmg ~/Desktop/Catalina.dmg
hdiutil detach /Volumes/Install\ macOS\ Catalina
hdiutil convert ~/Desktop/Catalina.dmg -format UDTO -o ~/Desktop/Catalina.iso
mv ~/Desktop/Catalina.iso.cdr ~/Desktop/Catalina.iso
rm ~/Desktop/Catalina.dmg
