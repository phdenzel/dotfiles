#!/usr/bin/osascript
tell application "System Preferences"
  reveal anchor "keyboardTab" of pane "com.apple.preference.keyboard"
end tell

tell application "System Events" to tell window 1 of process "System Preferences"
  click button "Modifier Keys…" of tab group 1
  tell sheet 1
    tell pop up button 2
      click
      click menu item 2 of menu 1
    end tell
    click button "OK"
  end tell
end tell

quit application "System Preferences"
