import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Data.List

myManageHook = composeAll . concat $
   [ [ className =? "Firefox-bin" --> doShift "web" ]
   , [ className =? "emacs"       --> doShift "code" ]
   , [ className =? "Evince"      --> doShift "pdf" ]
   , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]

     -- using list comprehensions and partial matches
   , [ className =?  c --> doFloat | c <- myFloatsC ]
   ]
  where myFloatsC = ["Xmessage"]

-- Main pane in Tall is 2/3's of screen
myLayoutHook = tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = smartBorders $ avoidStruts $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 2/3

     -- Percent of screen to increment by when resizing panes
     delta   = 1/100


main = do
  xmproc <- spawnPipe "/home/keegan/bin/xmobar /home/keegan/.xmonad/xmobarrc"
  xmonad $ defaultConfig
       { manageHook = manageDocks <+> manageHook defaultConfig <+> myManageHook
       , layoutHook = myLayoutHook
       , logHook = dynamicLogWithPP $ xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 80
                   }
       , modMask = mod4Mask
       , focusFollowsMouse = False
       , workspaces = ["web", "code"] ++ map show [3..7] ++ ["pdf", "misc"]
       } `additionalKeys`
       [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
       , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
       , ((0, xK_Print), spawn "scrot")
       , ((mod4Mask, xK_c), spawn "/home/keegan/bin/mpris-remote playpause")
       , ((mod4Mask, xK_v), spawn "/home/keegan/bin/mpris-remote next")
       ]
