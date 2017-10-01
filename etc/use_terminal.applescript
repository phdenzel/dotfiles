#!/usr/bin/osascript

tell application "Terminal"
  local allOpenedWindows
  local initialOpenedWindows
  local windowID
  set themeName to "phd"

  (* Store the IDs of all the open Terminal windows *)
  set initialOpenedWindows to id of every window

  (* Open custom theme... the rest will be loaded automatically *)
  do shell script "cd; open '../../Documents/" & themeName & ".terminal'; cd -"

  (* Wait until custom theme is loaded *)
  delay 1

  (* Set theme as default *)
  set default settings to settings set themeName

  (* Get all currently opened windows *)
  set allOpenedWindows to id of every window

  repeat with windowID in allOpenedWindows
    (* Close additional windows that were opened *)
    if initialOpenedWindows does not contain windowID then
      close (every window whose id is windowID)
    (* Change theme for initially opened windows *)
    else
      set current settings of tabs of (every window whose id is windowID) to settings set themeName
    end if
  end repeat
end tell
