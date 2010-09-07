import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Data.List

myManageHook = composeAll . concat $
   [ [ className =? "Firefox-bin" --> doShift "web" ]
   , [ className =? "Emacs"       --> doShift "code" ]
   , [ className =? "Evince"      --> doShift "pdf" ]
   , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]

     -- using list comprehensions and partial matches
   , [ className =?  c --> doFloat | c <- myFloatsC ]
   , [ title     =?  t --> doFloat | t <- myFloatsT ]
   ]
  where myFloatsC = ["Xmessage", "display", "Gimp"]
        myFloatsT = ["Downloads", "VLC media player", "VLC (XVideo output)", "Save As...", "Open"]


myLayoutHook = smartBorders $ avoidStruts $ toggleLayouts
               (resize ||| tiled3 ||| grid ||| spiral (6/7))
               (tiled ||| full)
  where
     -- Normal Layouts
     tiled   = Tall nmaster delta ratio
     full    = Full

     -- Other Layouts
     resize  = ResizableTall nmaster delta ratio []
     tiled3  = ThreeCol nmaster delta (1/2)
     grid    = Grid

     -- Common Parameters
     nmaster = 1
     ratio   = 2/3
     delta   = 1/100


-- hook for xmobar to change titles of layouts
layoutName "Tall" = "Two Columns"
layoutName "Full" = "Fullscreen"
layoutName "ResizableTall" = "Resizable Two Columns"
layoutName "ThreeCol" = "Three Columns"
layoutName s = s


main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ defaultConfig
       { manageHook = manageDocks <+> manageHook defaultConfig <+> myManageHook
       , layoutHook = myLayoutHook
       , logHook = dynamicLogWithPP $ xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 80
                   , ppLayout = layoutName
                   }
       , modMask = mod4Mask
       , workspaces = ["web", "code"] ++ map show [3..7] ++ ["pdf", "misc"]
       } `additionalKeys`
       [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock")

       -- Screenshots
       , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
       , ((0, xK_Print), spawn "scrot")

       -- Toggle between common layouts and other layouts
       , ((mod4Mask .|. shiftMask, xK_space), sendMessage ToggleLayout)


       -- ResizableTall keybindings
       , ((mod4Mask, xK_i), sendMessage MirrorShrink)
       , ((mod4Mask, xK_u), sendMessage MirrorExpand)


       -- Multimedia shortcuts
       , ((mod4Mask, xK_c), spawn "mpc toggle")
       , ((mod4Mask, xK_v), spawn "mpc next")

       -- multimedia keys
       --
       -- XF86AudioLowerVolume
       , ((0, 0x1008ff11), spawn "amixer -q set Master 5%-")
       -- XF86AudioRaiseVolume
       , ((0, 0x1008ff13), spawn "amixer -q set Master 5%+")
       -- XF86AudioMute
       , ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
       -- XF86AudioPlay
       , ((0, 0x1008ff14), spawn "mpc playpause")
       ]
