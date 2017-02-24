{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.

  If you want to customize this file, the easiest workflow goes
  something like this:
    1. Make a small change.
    2. Hit "super-q", which recompiles and restarts xmonad
    3. If there is an error, undo your change and hit "super-q" again to
       get to a stable place again.
    4. Repeat

  Author:     David Brewer
  Repository: https://github.com/davidbrewer/xmonad-ubuntu-conf
-}

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Layout.Renamed (renamed, Rename(Replace, Prepend))
import XMonad.Layout.GridVariants

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.EwmhDesktops -- in an attempt to get xdotool working
import XMonad.Actions.Navigation2D

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))

{-
  Key Aliases

  find a key using command line: `xev`

  also useful: `xmodmap`

    shift       Shift_L (0x32),  Shift_R (0x3e)
    lock      
    control     Control_L (0x25),  Control_L (0x42),  Control_R (0x69)
    mod1        Alt_L (0x40),  Alt_R (0x6c),  Meta_L (0xcd)
    mod2        Num_Lock (0x4d)
    mod3      
    mod4        Super_L (0x85),  Super_R (0x86),  Super_L (0xce),  Hyper_L (0xcf)
    mod5        ISO_Level3_Shift (0x5c),  Mode_switch (0xcb)

-}

altMask = mod1Mask -- I'm not using these aliases yet, but, that's what they mean
windMask    = mod4Mask

{-
  Xmonad configuration variables. These settings control some of the
  simpler parts of xmonad's behavior and are straightforward to tweak.
-}

myModMask            = mod4Mask       -- changes the mod key to "super"
myFocusedBorderColor = "#ff6644"      -- color of focused border
myNormalBorderColor  = "#aaaaaa"      -- color of inactive border
myBorderWidth        = 5              -- width of border around windows
myTerminal           = "gnome-terminal"     -- which terminal software to use


{-
  Xmobar configuration variables. These settings control the appearance
  of text which xmonad is sending to xmobar via the DynamicLog hook.
-}

myTitleColor     = "#eeeeee"  -- color of window title
myTitleLength    = 80         -- truncate window title to this length
myCurrentWSColor = "#e6744c"  -- color of active workspace
myVisibleWSColor = "#c185a7"  -- color of inactive workspace
myUrgentWSColor  = "#cc0000"  -- color of workspace with 'urgent' window
myCurrentWSLeft  = "["        -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft  = "("        -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft  = "{"         -- wrap urgent workspace with these
myUrgentWSRight = "}"


{-
  Workspace configuration. Here you can change the names of your
  workspaces. Note that they are organized in a grid corresponding
  to the layout of the number pad.

  I would recommend sticking with relatively brief workspace names
  because they are displayed in the xmobar status bar, where space
  can get tight. Also, the workspace labels are referred to elsewhere
  in the configuration file, so when you change a label you will have
  to find places which refer to it and make a change there as well.

  This central organizational concept of this configuration is that
  the workspaces correspond to keys on the number pad, and that they
  are organized in a grid which also matches the layout of the number pad.
  So, I don't recommend changing the number of workspaces unless you are
  prepared to delve into the workspace navigation keybindings section
  as well.
-}

myWorkspaces =
  [
    "7", "8", "9",
    "4", "5", "6",
    "1", "2", "3",
    "0", ".", "ent"
  ]

{-
  Layout configuration. In this section we identify which xmonad
  layouts we want to use. I have defined a list of default
  layouts which are applied on every workspace, as well as
  special layouts which get applied to specific workspaces.

  Note that all layouts are wrapped within "avoidStruts". What this does
  is make the layouts avoid the status bar area at the top of the screen.
  Without this, they would overlap the bar. You can toggle this behavior
  by hitting "super-b" (bound to ToggleStruts in the keyboard bindings
  in the next section).
-}

-- Define group of default layouts used on most screens, in the
-- order they will appear.
-- "smartBorders" modifier makes it so the borders on windows only
-- appear if there is more than one visible window.
-- "avoidStruts" modifier makes it so that the layout provides
-- space for the status bar at the top of the screen.
defaultLayouts = smartBorders(avoidStruts(
  -- ResizableTall layout has a large master window on the left,
  -- and remaining windows tile on the right. By default each area
  -- takes up half the screen, but you can resize using "super-y" and
  -- "super-o".
  (renamed [Replace "1-Split"] $ ResizableTall 1 (5/100) (2/3) [])

  -- Split Grids
  -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-GridVariants.html
  ||| (renamed [Replace "2-Split"] $ SplitGrid XMonad.Layout.GridVariants.L 1 2 (9/10) (16/10) (5/100))
  ||| (renamed [Replace "3-Split"] $ SplitGrid XMonad.Layout.GridVariants.L 1 3 (9/10) (16/10) (5/100))
  
  -- Full layout makes every window full screen. When you toggle the
  -- active window, it will bring the active window to the front.
  ||| noBorders Full

  ))


-- Here we define some layouts which will be assigned to specific
-- workspaces based on the functionality of that workspace.

-- The GIMP layout uses the ThreeColMid layout. The traditional GIMP
-- floating panels approach is a bit of a challenge to handle with xmonad;
-- I find the best solution is to make the image you are working on the
-- master area, and then use this ThreeColMid layout to make the panels
-- tile to the left and right of the image. If you use GIMP 2.8, you
-- can use single-window mode and avoid this issue.

-- gimpLayout = smartBorders(avoidStruts(ThreeColMid 1 (3/100) (3/4)))

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayouts = avoidStruts $ defaultLayouts -- layoutHook defaultConfig -- defaultLayouts

{-
  Custom keybindings. In this section we define a list of relatively
  straightforward keybindings. This would be the clearest place to
  add your own keybindings, or change the keys we have defined
  for certain functions.

  It can be difficult to find a good list of keycodes for use
  in xmonad. I have found this page useful -- just look
  for entries beginning with "xK":

  http://xmonad.org/xmonad-docs/xmonad/doc-index-X.html

  Note that in the example below, the last three entries refer
  to nonstandard keys which do not have names assigned by
  xmonad. That's because they are the volume and mute keys
  on my laptop, a Lenovo W520.

  If you have special keys on your keyboard which you
  want to bind to specific actions, you can use the "xev"
  command-line tool to determine the code for a specific key.
  Launch the command, then type the key in question and watch
  the output.
-}

myKeyBindings =
  [ -- XMobar
    ((myModMask, xK_b), sendMessage ToggleStruts)

  -- App Launcher
  , ((myModMask, xK_p), spawn "rofi -run-command \"/bin/bash -c -i '{cmd}'\" -show run") -- uses g_spawn_async 
  -- , ((myModMask, xK_p), spawn "rofi -show run")
  -- rofi -run-list-command "/home/josh/.xmonad/rofi-aliases.sh" -run-command "/bin/bash -i -c '{cmd}'" -rnow -show run

  , ((myModMask, xK_u), focusUrgent)

  -- Audio: relevant to my desktop
  , ((myModMask, xK_F10), spawn "amixer -q -D pulse set Master toggle")
  , ((myModMask, xK_F11), spawn "amixer -q -D pulse set Master 10%-")
  , ((myModMask, xK_F12), spawn "amixer -q -D pulse set Master 10%+")    

  -- -- relevant to laptops
  -- , ((0, 0x1008FF12), spawn "amixer -q -D pulse set Master toggle")
  -- , ((0, 0x1008FF11), spawn "amixer -q -D pulse set Master 10%-")
  -- , ((0, 0x1008FF13), spawn "amixer -q -D pulse set Master 10%+")
  ]


{-
  Management hooks. You can use management hooks to enforce certain
  behaviors when specific programs or windows are launched. This is
  useful if you want certain windows to not be managed by xmonad,
  or sent to a specific workspace, or otherwise handled in a special
  way.

  Each entry within the list of hooks defines a way to identify a
  window (before the arrow), and then how that window should be treated
  (after the arrow).

  To figure out to identify your window, you will need to use a
  command-line tool called "xprop". When you run xprop, your cursor
  will temporarily change to crosshairs; click on the window you
  want to identify. In the output that is printed in your terminal,
  look for a couple of things:
    - WM_CLASS(STRING): values in this list of strings can be compared
      to "className" to match windows.
    - WM_NAME(STRING): this value can be compared to "resource" to match
      windows.

  The className values tend to be generic, and might match any window or
  dialog owned by a particular program. The resource values tend to be
  more specific, and will be different for every dialog. Sometimes you
  might want to compare both className and resource, to make sure you
  are matching only a particular window which belongs to a specific
  program.

  Once you've pinpointed the window you want to manipulate, here are
  a few examples of things you might do with that window:
    - doIgnore: this tells xmonad to completely ignore the window. It will
      not be tiled or floated. Useful for things like launchers and
      trays.
    - doFloat: this tells xmonad to float the window rather than tiling
      it. Handy for things that pop up, take some input, and then go away,
      such as dialogs, calculators, and so on.
    - doF (W.shift "Workspace"): this tells xmonad that when this program
      is launched it should be sent to a specific workspace. Useful
      for keeping specific tasks on specific workspaces. In the example
      below I have specific workspaces for chat, development, and
      editing images.
-}

myManagementHooks :: [ManageHook]
myManagementHooks = [
  -- resource =? "albert" --> doIgnore
  --, resource =? "synapse" --> doIgnore
  resource =? "stalonetray" --> doIgnore
  -- , className =? "rdesktop" --> doFloat
  -- , (className =? "Komodo IDE") --> doF (W.shift "5:Dev")
  -- , (className =? "Komodo IDE" <&&> resource =? "Komodo_find2") --> doFloat
  -- , (className =? "Komodo IDE" <&&> resource =? "Komodo_gotofile") --> doFloat
  -- , (className =? "Komodo IDE" <&&> resource =? "Toplevel") --> doFloat
  -- , (className =? "Empathy") --> doF (W.shift "7:Chat")
  -- , (className =? "Pidgin") --> doF (W.shift "7:Chat")
  -- , (className =? "Gimp-2.8") --> doF (W.shift "9:Pix")
  ]


{-
  Workspace navigation keybindings. This is probably the part of the
  configuration I have spent the most time messing with, but understand
  the least. Be very careful if messing with this section.
-}

-- We define two lists of keycodes for use in the rest of the
-- keyboard configuration. The first is the list of numpad keys,
-- in the order they occur on the keyboard (left to right and
-- top to bottom). The second is the list of number keys, in an
-- order corresponding to the numpad. We will use these to
-- make workspace navigation commands work the same whether you
-- use the numpad or the top-row number keys. And, we also
-- use them to figure out where to go when the user
-- uses the arrow keys.
numPadKeys =
  [
    xK_KP_Home, xK_KP_Up, xK_KP_Page_Up
    , xK_KP_Left, xK_KP_Begin,xK_KP_Right
    , xK_KP_End, xK_KP_Down, xK_KP_Page_Down
    , xK_KP_Insert, xK_KP_Delete, xK_KP_Enter
  ]

numKeys =
  [
    xK_7, xK_8, xK_9
    , xK_4, xK_5, xK_6
    , xK_1, xK_2, xK_3
    , xK_0, xK_minus, xK_equal
  ]

altNumKeys =
  [
    xK_w, xK_e, xK_r
    , xK_s, xK_d, xK_f
    , xK_x, xK_c, xK_v
  ]  

-- Here, some magic occurs that I once grokked but has since
-- fallen out of my head. Essentially what is happening is
-- that we are telling xmonad how to navigate workspaces,
-- how to send windows to different workspaces,
-- and what keys to use to change which monitor is focused.
myKeys = myKeyBindings ++
  [ -- for when NumLock is set to the key's symbols
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numPadKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [ -- for when NumLock is set to the key's numbers
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [ -- for when NumLock is set to the key's numbers
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces altNumKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  M.toList (planeKeys myModMask (Lines 4) Finite) ++         -- ???
  [ -- for changing monitors
    ((m .|. myModMask .|. mod1Mask, key), screenWorkspace sc -- mod1Mask = altMask
      >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_h, xK_l] [1,0]                  -- change monitors
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]        -- move to monitor
  ]

  -- Swap Monitors
  ++ [((myModMask, xK_a), swapNextScreen)]

  -- Screenshots:
  --   mod-Print   = all screens
  --   mod-C-Print = just window
  ++ [ ((myModMask, xK_Print),                 
        spawn "scrot \"/home/josh/Media/Pictures/0 screenshots/screen_%Y-%m-%d-%H-%M-%S.png -d 1\"")
     , ((myModMask .|. controlMask, xK_Print), 
        spawn "scrot \"/home/josh/Media/Pictures/0 screenshots/window_%Y-%m-%d-%H-%M-%S.png\" -u")]

  -- Kill Currently Focused Window
  ++ [ ((myModMask .|. shiftMask, xK_z), kill) ]

  -- Expand/Shrink Master
  ++ [ ((myModMask, xK_y), sendMessage Shrink)
     , ((myModMask, xK_o), sendMessage Expand)]
  
  -- Window Focus, ala Vim
  --   trailing Bool asks if it should wrap at window edges
  ++ [ ((myModMask, xK_j), windowGo XMonad.Actions.Navigation2D.D False)
     , ((myModMask, xK_k), windowGo XMonad.Actions.Navigation2D.U False)
     , ((myModMask, xK_h), windowGo XMonad.Actions.Navigation2D.L False)
     , ((myModMask, xK_l), windowGo XMonad.Actions.Navigation2D.R False)]

  -- Swap Windows
  ++ [ ((myModMask .|. shiftMask, xK_j), windowSwap XMonad.Actions.Navigation2D.D False)
     , ((myModMask .|. shiftMask, xK_k), windowSwap XMonad.Actions.Navigation2D.U False)
     , ((myModMask .|. shiftMask, xK_h), windowSwap XMonad.Actions.Navigation2D.L False)
     , ((myModMask .|. shiftMask, xK_l), windowSwap XMonad.Actions.Navigation2D.R False)]


{-
  Here we actually stitch together all the configuration settings
  and run xmonad. We also spawn an instance of xmobar and pipe
  content into it via the logHook.
-}

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"

  xmonad
    $ ewmh
    $ withUrgencyHook NoUrgencyHook
    $ def {
    focusedBorderColor = myFocusedBorderColor
  , focusFollowsMouse = False
  , normalBorderColor = myNormalBorderColor
  , terminal = myTerminal
  , borderWidth = myBorderWidth
  , layoutHook = myLayouts
  , workspaces = myWorkspaces
  , modMask = myModMask

    -- NOTE: I added in Ewmh to try to get xdotool working, screen-manipulation-from-terminal kind of stuff
  , handleEventHook = XMonad.Layout.Fullscreen.fullscreenEventHook <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook
  , startupHook = do
      setWMName "LG3D" -- i think this is my starting window, not sure
      
      spawn "~/.xmonad/startup-hook"
  , manageHook = manageHook def
      <+> composeAll myManagementHooks
      <+> manageDocks

  -- For Xmobar
  , logHook = takeTopFocus <+> dynamicLogWithPP xmobarPP {
      ppOutput    = hPutStrLn xmproc
      , ppTitle   = xmobarColor myTitleColor "" . shorten myTitleLength
      , ppCurrent = xmobarColor myCurrentWSColor ""
        . wrap myCurrentWSLeft myCurrentWSRight
      , ppVisible = xmobarColor myVisibleWSColor ""
        . wrap myVisibleWSLeft myVisibleWSRight
      , ppUrgent  = xmobarColor myUrgentWSColor ""
        . wrap myUrgentWSLeft myUrgentWSRight
      }
  }
    `additionalKeys` myKeys
