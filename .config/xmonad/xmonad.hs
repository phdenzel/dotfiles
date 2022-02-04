--------------------
-- XMonad configurations
--------------------

-------------------- Imports
-- Base
import XMonad
import System.IO
import System.Exit
import qualified XMonad.StackSet as W
-- Data
import Data.Monoid
import qualified Data.Map as M
-- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (docks, manageDocks, avoidStruts,
                                 ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,
                                   doFullFloat, doCenterFloat)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP (wrap, shorten, xmobarColor, xmobarBorder,
                                  PP(..))
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.SetWMName
-- Layout
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ResizableTile (ResizableTall(..), MirrorResize(MirrorShrink, MirrorExpand))
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableThreeColumns (ResizableThreeCol(ResizableThreeColMid))
import XMonad.Layout.NoBorders (noBorders, smartBorders, withBorder)
import XMonad.Layout.Spacing (spacingRaw, Spacing(..), Border(..))
import XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import XMonad.Layout.ShowWName
-- Actions
import XMonad.Actions.CycleWS (moveTo, shiftTo, nextScreen, prevScreen,
                               anyWS, ignoringWSs,
                               Direction1D(Next, Prev), WSType(WSIs, (:&:)))
import XMonad.Actions.WithAll (sinkAll, killAll)
-- Utils
-- import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (mkKeymap, checkKeymap)
import XMonad.Util.Run (safeSpawn, hPutStrLn)
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.NamedScratchpad
--- Customized colors
import Colors.PhDDark  -- color[Trayer, Fore, Back, 01..15]


-------------------- Variables
-- Default programs
myXMobar :: String
myXMobar = "xmobar"
myXMobarConf :: String
myXMobarConf = "~/.config/xmobar/xmobarrc"
myTerminal :: String
myTerminal = "alacritty"
myBrowser  :: String
myBrowser = "brave"
myEmacs :: String
myEmacs = "emacsclient -c --alternate-editor='emacs'"
myEditor :: String
myEditor = "emacsclient -c --alternate-editor='emacs'"
-- Style config
myBorderWidth :: Dimension
myBorderWidth = 2
myFont :: String
myFont = "xft:Fira Mono:regular:size=9:antialias=true:hinting=true"
myNormalColor :: String
myNormalColor = colorBack
myFocusColor :: String
myFocusColor = color06
-- Mouse controls
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
myClickJustFocuses :: Bool
myClickJustFocuses = True
-- Key controls
myModMask :: KeyMask
myModMask = mod4Mask  -- Super-mod4Mask | L-Alt-mod1Mask | R-Alt-mod3Mask


-------------------- Main
main :: IO ()
main = do
  xmonad $
    ewmhFullscreen . ewmh . docks $
    dynamicSBs xmobarSpawn myConfigs


myConfigs = def
    -- simple stuff
  { terminal           = myTerminal
  , focusFollowsMouse  = myFocusFollowsMouse
  , clickJustFocuses   = myClickJustFocuses
  , borderWidth        = myBorderWidth
  , modMask            = myModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalColor
  , focusedBorderColor = myFocusColor

    -- key bindings
  , keys               = myKeys
  , mouseBindings      = myMouseBindings

    -- hooks, layouts
  , startupHook        = myStartupHook
  , layoutHook         = myLayoutHook  -- showWName' myShowWNameTheme $ 
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , logHook            = myLogHook
}


-------------------- Startup hook
myStartupHook :: X()
myStartupHook = do
  spawn     "killall trayer"
  spawnOnce "resolution_x11 &"                 -- set screen resolution using xrandr
  spawnOnce "xsetroot -cursor_name left_ptr &" -- set cursor
  spawnOnce "xset r rate 180 35 &"             -- increase scroll speed
  spawnOnce "xrgb -merge ~/.Xresources &"      -- load x resources
  spawnOnce "xmodmap ~/.Xmodmap &"             -- load x modmap
  spawnOnce "picom &"                          -- start compositor
  spawnOnce "~/.config/feh/fehbg &"            -- set wallpaper
  spawnOnce "xscreensaver -no-splash &"        -- xscreensaver daemon
  spawnOnce "/usr/bin/emacs --daemon &"        -- Emacs daemon
  spawn     ("trayer --edge top --align right --widthtype request "
             ++ "--padding 6 --SetDockType true --SetPartialStrut true "
             ++ "--expand true --transparent true --alpha 0 --height 30 "
             ++ "--iconspacing 16 "
             ++ colorTrayer
             ++ "&"
            )
  spawn     "blueman-applet &"
  spawn     "nm-applet &"
  setWMName "LG3D"  -- Java hack
  return () >> checkKeymap myConfigs myKeymap


-------------------- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["wm", "tty", "dev", "web", "doc", "mu", "tx", "gx", "ls"]
-- myWorkspaces    = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
-- myWorkspaces = ["<fn=3>\xf036</fn>", "<fn=3>\xf120</fn>", "<fn=3>\xf121</fn>",
--                 "<fn=3>\xf7a2</fn>", "<fn=3>\xf01c</fn>", "<fn=3>\xf1c0</fn>",
--                 "<fn=3>\xf56b</fn>", "<fn=3>\xf441</fn>", "<fn=3>\xf038</fn>"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

myShowWNameTheme :: SWNConfig  -- for indicators when switching workspaces
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=48"
    , swn_fade              = 1.0
    , swn_bgcolor           = colorBack
    , swn_color             = color07
    }

windowCount :: X (Maybe String)  -- count open windows on workspaces
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


-------------------- Layouts
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall = renamed [Replace "tall"]
       $ avoidStruts
       $ smartBorders
       $ mySpacing 3
       $ ResizableTall nmaster delta ratio []
  where
    nmaster = 1   -- number of master pane windows
    ratio = 1/2   -- area ratio of master pane
    delta = 3/100 -- percentual resizing increment

grid = renamed [Replace "grid"]
       $ avoidStruts
       $ smartBorders
       $ mySpacing 3
       $ Grid (aspect)
  where
    aspect = 16/10  -- desired aspect ratio of windows

mirr = renamed [Replace "mirr"]
       $ avoidStruts
       $ smartBorders
       $ mySpacing 3
       $ Mirror
       $ ResizableTall nmaster delta ratio []
  where
    nmaster = 1
    ratio = 1/2
    delta = 3/100

c3s = renamed [Replace "c3s"]
      $ avoidStruts
      $ smartBorders
      $ mySpacing 3
      $ ResizableThreeColMid nmaster delta ratio []
  where
    nmaster = 1
    ratio = 1/2
    delta = 3/100

full = renamed [Replace "full"]
       $ avoidStruts
       $ noBorders
       $ Full
        
myLayoutHook = (tall ||| grid ||| mirr ||| c3s ||| full)

-------------------- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "calculator" spawnCalc findCalc manageCalc
                , NS "ranger" spawnRanger findRanger manageRanger
                ]
  where

    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 -h
        l = 0.70 -w

    spawnRanger  = myTerminal ++ " --class ranger -t Ranger -e ranger"
    findRanger   = appName =? "ranger"
    manageRanger = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w


-------------------- Manage windows
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = (composeAll . concat $
                -- class-based management
                [ [className =? c <||> title =?
                                c --> doShift (myWorkspaces !! 0) | c <- mywmShifts ]
                , [className =? c --> doShift (myWorkspaces !! 1) | c <- myttyShifts]
                , [className =? c --> doShift (myWorkspaces !! 2) | c <- mydevShifts]
                , [className =? c --> doShift (myWorkspaces !! 3) | c <- mywebShifts]
                , [className =? c --> doShift (myWorkspaces !! 4) | c <- mydocShifts]
                , [className =? c --> doShift (myWorkspaces !! 5) | c <- mymuShifts ]
                , [className =? c --> doShift (myWorkspaces !! 6) | c <- mytxShifts ]
                , [className =? c --> doShift (myWorkspaces !! 7) | c <- mygxShifts ]
                , [className =? c --> doShift (myWorkspaces !! 8) | c <- mylsShifts ]
                , [className =? c --> doFullFloat                 | c <- myfFloats  ]
                , [className =? c --> doCenterFloat               | c <- mycFloats  ]
                , [resource =?  r --> doIgnore                    | r <- myIgnores  ]
                -- situational management
                , [ isFullscreen  --> doFullFloat  ]
                , [ isDialog      --> doCenterFloat]
                ])
               <+> namedScratchpadManageHook myScratchPads
               -- <+> fullscreenManageHook
               <+> manageDocks
               <+> manageHook def
  where
    mywmShifts  = [ "" ]
    myttyShifts = [ "Xterm" ]
    mydevShifts = [ "Emacs", "Code" ]
    mywebShifts = [ "Brave-browser" ]
    mydocShifts = [ "Pcmanfm", "Ranger" ]
    mymuShifts  = [ "Mailspring" ]
    mytxShifts  = [ "Transmission-gtk" ]
    mygxShifts  = [ "Gimp" ]
    mylsShifts  = [ "" ]
    myfFloats   = [ "" ]
    mycFloats   = [ "feh", "Xmessage" ]
    myIgnores   = [ "desktop_window", "kdesktop" ]


-------------------- Event handling
myEventHook = mempty --  mconcat [ fullscreenEventHook, handleEventHook def ]


-------------------- xmobar
myXMobarPP :: PP
myXMobarPP = def
  { ppSep     = wrap hair hair $ grey "|"
  --, ppWsSep   = wrap hair hair $ blue "/"
  -- focused workspace
  , ppCurrent = red . xmobarBorder "Bottom" redHex 5
  , ppVisible = red
  -- hidden workspace with windows
  , ppHidden  = blue . xmobarBorder "Top" blueHex 3 . hideNSP
  -- hidden windows without windows
  , ppHiddenNoWindows = blue . hideNSP
  -- layout format map
  , ppLayout  = cyan . (\layout -> case layout of
                           "tall" -> "{|}"
                           "grid" -> "[#]"
                           "mirr" -> "}|{"
                           "c3s"  -> "|||"
                           "full" -> "[X]")
  -- window count
  , ppExtras  = [ windowCount ] -- xmobarColor color03 ""
  -- order pp fields
  -- , ppTitle   = xmobarColor color07 "" . shorten 25
  , ppOrder   = \(ws:l:t:ex) -> [ws,l] ++ map red ex
  }
  where
    hideNSP :: WorkspaceId -> String
    hideNSP ws = if ws /= "NSP" then ws else ""
    greyHex, blueHex, redHex, cyanHex :: String
    greyHex = color08
    blueHex = color04
    redHex = color05
    cyanHex = color06
    blue, red, cyan, grey :: String -> String
    blue = xmobarColor blueHex ""
    red = xmobarColor redHex ""
    cyan = xmobarColor cyanHex ""
    grey = xmobarColor greyHex ""
    hair :: String
    hair = "<fn=1> </fn>"

xmobar0 = statusBarPropTo "_XMONAD_LOG_1" (myXMobar++" -x 0 "++myXMobarConf) (pure myXMobarPP)
xmobarSpawn :: ScreenId -> IO StatusBarConfig
xmobarSpawn 0 = pure $ xmobar0


-------------------- Logging
myLogHook = refocusLastLogHook
            >> nsHideOnFocusLoss myScratchPads


-------------------- Keybindings
myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = (mkKeymap conf myKeymap) <+> (defaultKeymap conf)
myKeymap :: [(String, X ())]
myKeymap =
  -- Launch/kill bindings
  [ ("M-<Return>"   , spawn myTerminal)
  , ("M-/"          , spawn "dmenu_run -c -l 15")
  , ("M-p"          , spawn "passmenu")
  , ("M-S-c"        , kill)
  , ("M-C-c"        , kill)
  , ("M-S-C-c"      , killAll)

  -- XMonad & system bindings
  , ("M-b"          , sendMessage ToggleStruts)  -- toggle status bar
  , ("M-S-b"        , spawn "xmobar_toggle")     -- kill status bar
  , ("M-q"          , spawn "xmonad_restart")    -- recompile & restart xmonad
  , ("M-S-x"        , io (exitWith ExitSuccess)) -- exit XMonad
  , ("M-S-z"        , spawn "xscreensaver-command --activate")  -- suspend
  , ("M-S-v"        , spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

  -- Window control
  , ("M-<Tab>"      , windows W.focusDown)
  , ("M-j"          , windows W.focusDown)
  , ("M-S-j"        , windows W.swapDown)
  , ("M-k"          , windows W.focusUp)
  , ("M-S-k"        , windows W.swapUp)
  , ("M-m"          , windows W.focusMaster)
  , ("M-S-m"        , windows W.swapMaster)
  , ("M-n"          , refresh)
  , ("M-h"          , sendMessage Shrink)
  , ("M-l"          , sendMessage Expand)
  , ("M-C-h"        , moveTo Prev (nonNSP))
  , ("M-C-l"        , moveTo Next (nonNSP))
  , ("M-S-h"        , shiftTo Prev (nonNSP) >> moveTo Prev (nonNSP))
  , ("M-S-l"        , shiftTo Next (nonNSP) >> moveTo Next (nonNSP))
  , ("M-<Down>"     , windows W.focusDown)
  , ("M-S-<Down>"   , windows W.swapDown)
  , ("M-<Up>"       , windows W.focusUp)
  , ("M-S-<Up>"     , windows W.swapUp)
  , ("M-<Left>"     , sendMessage Shrink)
  , ("M-<Right>"    , sendMessage Expand)
  , ("M-C-<Up>"     , sendMessage MirrorExpand)
  , ("M-C-<Down>"   , sendMessage MirrorShrink)
  , ("M-C-<Left>"   , moveTo Prev (nonNSP))
  , ("M-C-<Right>"  , moveTo Next (nonNSP))
  , ("M-S-<Left>"   , shiftTo Prev (nonNSP) >> moveTo Prev (nonNSP))
  , ("M-S-<Right>"  , shiftTo Next (nonNSP) >> moveTo Next (nonNSP))
  , ("M-,"          , nextScreen)
  , ("M-."          , prevScreen)

  -- Toggle layouts
  , ("M-<Space>"    , sendMessage NextLayout)
  , ("M-S-<Space>"  , sendMessage FirstLayout)
  , ("M-f"          , sendMessage (JumpToLayout "bfull") >> sendMessage ToggleStruts)
  , ("M-S-f"        , withFocused $ float)
  , ("M-t"          , withFocused $ windows . W.sink)
  , ("M-S-t"        , sinkAll)
  
  -- Program bindings
  , ("M-d"          , spawn "pcmanfm")
  , ("M-\\"         , spawn myBrowser)
  , ("M-="          , unGrab *> spawn "scrot")
  , ("M-S-="        , unGrab *> spawn "scrot -s")

  -- Scratchpads
  , ("M-S-<Return>" , namedScratchpadAction myScratchPads "terminal")
  , ("M-S-y"        , namedScratchpadAction myScratchPads "calculator")
  , ("M-S-d"        , namedScratchpadAction myScratchPads "ranger")
  ]
  where
    nonNSP = anyWS :&: ignoringWSs [scratchpadWorkspaceTag]

defaultKeymap conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- , ((0, button2), (\w -> focus w >> mouseMoveWindow w
    --                                 >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines [
  "The modifier key is 'Super'. Default keybindings:",
  "",
  "-- launching and killing programs",
  "mod-Shift-Enter  Launch xterminal",
  "mod-p            Launch dmenu",
  "mod-Shift-p      Launch gmrun",
  "mod-Shift-c      Close/kill the focused window",
  "mod-Space        Rotate through the available layout algorithms",
  "mod-Shift-Space  Reset the layouts on the current workSpace to default",
  "mod-n            Resize/refresh viewed windows to the correct size",
  "",
  "-- move focus up or down the window stack",
  "mod-Tab        Move focus to the next window",
  "mod-Shift-Tab  Move focus to the previous window",
  "mod-j          Move focus to the next window",
  "mod-k          Move focus to the previous window",
  "mod-m          Move focus to the master window",
  "",
  "-- modifying the window order",
  "mod-Return   Swap the focused window and the master window",
  "mod-Shift-j  Swap the focused window with the next window",
  "mod-Shift-k  Swap the focused window with the previous window",
  "",
  "-- resizing the master/slave ratio",
  "mod-h  Shrink the master area",
  "mod-l  Expand the master area",
  "",
  "-- floating layer support",
  "mod-t  Push window back into tiling; unfloat and re-tile it",
  "",
  "-- increase or decrease number of windows in the master area",
  "mod-comma  (mod-,)   Increment the number of windows in the master area",
  "mod-period (mod-.)   Deincrement the number of windows in the master area",
  "",
  "-- quit, or restart",
  "mod-Shift-q  Quit xmonad",
  "mod-q        Restart xmonad",
  "mod-[1..9]   Switch to workSpace N",
  "",
  "-- Workspaces & screens",
  "mod-Shift-[1..9]   Move client to workspace N",
  "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
  "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
  "",
  "-- Mouse bindings: default actions bound to mouse events",
  "mod-button1  Set the window to floating mode and move by dragging",
  "mod-button2  Raise the window to the top of the stack",
  "mod-button3  Set the window to floating mode and resize by dragging"]
