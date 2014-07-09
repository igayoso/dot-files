-- igayoso's xmonad.hs
-- Based on rupa's xmonad.hs, https://github.com/rupa/xmonad
-- Based on joe di castro's xmonad.hs, http://joedicastro.com/productividad-en-el-escritorio-linux-xmonad.html

-- ===============================================================  KEY BINDINGS

------------------------------------------------- Launch (or Raise) Applications
-- Win  +  Enter                   Terminal
-- Win  +  F1                      Nautilus
-- Win  +  F2                      Firefox                  (single instance)
----------------------------------------------------------- Shell/Window prompts
-- Win  +  Space                   Run or Raise Shell Prompt
-- Win  +  Shift   +  Space        Run Shell Prompt
-- Win  +  Control +  Space        Window Prompt
--------------------------------------------------------------------- Navigation
-- Win  +  [1..9]                  Switch to workspace N
-- Win  +  Shift   +  [1..9]       Move client to workspace N
-- Ctrl +  Alt     +  Left/Right   Previous/Next workspace
-- Win  +  j                       Focus next window
-- Win  +  k                       Focus previous window
-------------------------------------------------------------- Window management
-- Win  +  Shift   +  Left         Move window to previous workspace
-- Win  +  Shift   +  Right        Move window to next workspace
-- Win  +  Control +  Left         Move window to previous empty workspace
-- Win  +  Control +  Right        Move window to next empty workspace
-- Ctrl +  Up/Down                 Move focused window up/down
-- Win  +  m                       Toggle focused window Full Screen
-- Win  +  n                       Refresh
-- Win  +  -                       Move focused windows to master area
-- Win  +  w  (or Alt + F4)        Close focused window
-- Win  +  t                       Back to tiling (unfloat floating window)
-- Win  +  Shift   +  t            Back All to tiling (unfloat ALL windows)
-------------------------------------------------------------  Layout management
-- Win  +  Tab                     Rotate layouts
-- Win  +  Left/Right              Shrink/Expand the master area
-- Win  +  Up/Down                 Mirror Shrink/expand
-- Win  +  ,/.                     Increment/Deincrement 1 window in master area
-- Win  +  f                       Hide/Unhide the gnome-panel/status bar
-- Win  +  Shift   +  n            Reset current workspace to main layout
------------------------------------------------------------------ Mosaic Layout
-- Win  +  a/z                     Taller/Wider
-- Win  +  Control +  n            Reset
------------------------------------------------------------------------- Others
-- Print Screen                    Capture screen
-- Win  +  q                       Restart XMonad

-- ============================================================== MOUSE BINDINGS

-- Win  +  Button 1                Float Window and Move by dragging
-- Win  +  Button 2                Raise Window to the top
-- Win  +  Button 3                Float Window and Resize by dragging

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Monoid

import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.SinkAll
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger
import XMonad.Layout.Mosaic

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.XSelection

import XMonad.Config.Gnome

import XMonad.Util.EZConfig

-- Mod4 is the Super / Windows key
winMask = mod4Mask
altMask = mod1Mask

-- key bindings
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((winMask,                    xK_Return   ), spawn $ XMonad.terminal conf)
    , ((winMask,                    xK_F1       ), spawn "nautilus ~")
    , ((winMask,                    xK_F2       ), runOrRaise "firefox" (className =? "Firefox"))
    , ((winMask,                    xK_space    ), runOrRaisePrompt mySP)
    , ((winMask .|. shiftMask,      xK_space    ), shellPrompt mySP)
    , ((winMask .|. controlMask,    xK_space    ), windowPromptGoto mySP)
    , ((0,                          xK_Print    ), unsafeSpawn "gnome-screenshot")
    , ((altMask .|. controlMask,    xK_Right    ), moveTo Next (WSIs (return $ not . (=="NSP") . W.tag)))
    , ((altMask .|. controlMask,    xK_Left     ), moveTo Prev (WSIs (return $ not . (=="NSP") . W.tag)))
    , ((winMask .|. shiftMask,      xK_Right    ), shiftTo Next (WSIs (return $ not . (=="NSP") . W.tag)))
    , ((winMask .|. shiftMask,      xK_Left     ), shiftTo Prev (WSIs (return $ not . (=="NSP") . W.tag)))
    , ((winMask .|. controlMask,    xK_Right    ), shiftTo Next EmptyWS)
    , ((winMask .|. controlMask,    xK_Left     ), shiftTo Prev EmptyWS)
    , ((winMask,                    xK_Tab      ), sendMessage NextLayout >> (dynamicLogString myPP >>= \d->safeSpawn "gnome-osd-client" [d]))
    , ((winMask,                    xK_j        ), windows W.focusDown)
    , ((winMask,                    xK_k        ), windows W.focusUp)
    , ((controlMask,                xK_Down     ), windows W.swapDown)
    , ((controlMask,                xK_Up       ), windows W.swapUp)
    , ((winMask,                    xK_Left     ), sendMessage Shrink)
    , ((winMask,                    xK_Right    ), sendMessage Expand)
    , ((winMask,                    xK_Down     ), sendMessage MirrorShrink)
    , ((winMask,                    xK_Up       ), sendMessage MirrorExpand)
    , ((winMask,                    xK_minus    ), windows W.shiftMaster)
    , ((winMask,                    xK_comma    ), sendMessage (IncMasterN 1))
    , ((winMask,                    xK_period   ), sendMessage (IncMasterN (-1)))
    , ((winMask,                    xK_n        ), refresh)
    , ((winMask .|. shiftMask,      xK_n        ), setLayout $ XMonad.layoutHook conf)
    , ((winMask ,                   xK_a        ), sendMessage Taller)
    , ((winMask ,                   xK_z        ), sendMessage Wider)
    , ((winMask .|. controlMask,    xK_n        ), sendMessage Reset)
    , ((winMask,                    xK_m        ), sendMessage (Toggle "Full") >> (dynamicLogString myPP >>= \d->safeSpawn "gnome-osd-client" [d]))
    , ((winMask,                    xK_t        ), withFocused $ windows . W.sink)
    , ((winMask .|. shiftMask,      xK_t        ), sinkAll)
    , ((winMask,                    xK_f        ), sendMessage ToggleStruts)
    , ((winMask,                    xK_w        ), kill)
    , ((altMask,                    xK_F4       ), kill)
    , ((winMask,                    xK_q        ), broadcastMessage ReleaseResources >> restart "xmonad" True)
    , ((winMask,                    xK_l        ), spawn "slock")
    ]

    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. winMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ]

    ++
    -- sound special keys from thinkpad
    [ ((0, 0x1008ff12), spawn "amixer set Master toggle")
    , ((0, 0x1008ff11), spawn "amixer set Master 1000-")
    , ((0, 0x1008ff13), spawn "amixer set Master 1000+")
    ]

-- mouse bindings
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((winMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((winMask, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((winMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]

-- decoration theme
myDeco = defaultTheme
    { activeColor           = "orange"
    , inactiveColor         = "#222222"
    , urgentColor           = "yellow"
    , activeBorderColor     = "orange"
    , inactiveBorderColor   = "#222222"
    , urgentBorderColor     = "yellow"
    , activeTextColor       = "orange"
    , inactiveTextColor     = "#222222"
    , urgentTextColor       = "yellow"
    , decoHeight            = 10 }

-- tab theme
myTab = defaultTheme
    { activeColor           = "black"
    , inactiveColor         = "black"
    , urgentColor           = "yellow"
    , activeBorderColor     = "orange"
    , inactiveBorderColor   = "#222222"
    , urgentBorderColor     = "black"
    , activeTextColor       = "orange"
    , inactiveTextColor     = "#222222"
    , urgentTextColor       = "yellow" }

-- shell prompt theme
mySP = defaultXPConfig
    { bgColor               = "black"
    , fgColor               = "white"
    , bgHLight              = "gray"
    , fgHLight              = "black"
    , borderColor           = "orange"
    , promptBorderWidth     = 2
    , position              = Bottom
    , height                = 40
    --, autoComplete        = Just 1000
    , historySize           = 1000 }

-- dynamicLog theme (suppress everything but layout)
myPP = defaultPP
    { ppLayout  = (\ x -> case x of
      "Hinted ResizableTall"        -> "[|]"
      "Mirror Hinted ResizableTall" -> "[-]"
      "Hinted Tabbed Simplest"      -> "[T]"
      "Mosaic"                      -> "[M]"
      "Full"                 -> "[ ]"
      _                      -> x )
    , ppCurrent             = const ""
    , ppVisible             = const ""
    , ppHidden              = const ""
    , ppHiddenNoWindows     = const ""
    , ppUrgent              = const ""
    , ppTitle               = const ""
    , ppWsSep               = ""
    , ppSep                 = "" }

-- layouts
myLayout = avoidStruts $ toggleLayouts (noBorders Full)
    (smartBorders (tiled ||| mosaic 2 [3,2] ||| Mirror tiled ||| layoutHints (tabbed shrinkText myTab)))
    where
        tiled   = layoutHints $ ResizableTall nmaster delta ratio []
        nmaster = 1
        delta   = 2/100
        ratio   = 1/2

-- special windows
myManageHook = composeAll
    [ resource  =? "desktop_window"         --> doIgnore
    , className  =? "Xmessage"              --> doCenterFloat
    , className =? "Unity-2d-panel"         --> doIgnore
    , isFullscreen                          --> doFullFloat
    , isDialog                              --> doCenterFloat
    , resource =? "sun-awt-X11-XDialogPeer" --> doCenterFloat
    , title =? "screenkey"                  --> doIgnore
    , className =? "Gloobus-preview"        --> doCenterFloat
    , className =? "Clementine"             --> doShift "5:music"
    , ( isFullscreen --> doFullFloat ) <+> manageHook defaultConfig <+> manageDocks
    --                                      x y w h
    , scratchpadManageHook $ W.RationalRect 0 0 1 0.42
    , manageDocks ] <+> manageHook defaultConfig

-- let Gnome know about Xmonad actions
myLogHook = ewmhDesktopsLogHookCustom scratchpadFilterOutWorkspace >> updatePointer (Relative 0.5 0.5)

myConfig = ewmh defaultConfig
    { terminal           = "xterm"
    , borderWidth        = 2
    , normalBorderColor  = "black"
    , focusedBorderColor = "orange"
    , focusFollowsMouse  = True
    , modMask            = mod4Mask
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , layoutHook         = myLayout
    , manageHook         = myManageHook
--    , startupHook        = execScriptHook "startup" 
    }

main = do
xmproc <- spawnPipe "/usr/bin/xmobar /home/igayoso/.xmobarrc"
xmonad $ myConfig
    { logHook            = myLogHook }
