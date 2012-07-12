import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import System.IO
import Data.List
import qualified XMonad.StackSet as W
import Control.Monad
import Data.Monoid (All (All))


myManageHook = composeAll . concat $
   [ [ className =? "Firefox-bin" --> doShift "web" ]
   , [ className =? "Emacs"       --> doShift "code" ]
   , [ className =? "Evince"      --> doShift "pdf" ]
   , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]

     -- using list comprehensions and partial matches
   , [ className =?  c --> doFloat | c <- myFloatsC ]
   , [ title     =?  t --> doFloat | t <- myFloatsT ]
   , [ composeOne [ isFullscreen -?> doFullFloat ] ]
   ]
  where myFloatsC = ["Xmessage", "display", "Gimp", "Ediff"]
        myFloatsT = ["Downloads", "VLC media player", "VLC (XVideo output)",
                     "Save As...", "Open"]


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


-- Helper functions to fullscreen the window
-- Taken from http://code.google.com/p/xmonad/issues/attachmentText?id=339&aid=6617379484742651517&name=totemFullscreen.hs&token=a1509ad3307f95a43fc04357153a6c45
fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ W.float w r
    where r = W.RationalRect 0 0 1 1
tileWin w = windows $ W.sink w

evHook :: Event -> X All
evHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  isFull <- runQuery isFullscreen win

  -- Constants for the _NET_WM_STATE protocol
  let remove = 0
      add = 1
      toggle = 2

      -- The ATOM property type for changeProperty
      ptype = 4

      action = head dat

  when (typ == state && (fromIntegral fullsc) `elem` tail dat) $ do
    when (action == add || (action == toggle && not isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace [fromIntegral fullsc]
         fullFloat win
    when (head dat == remove || (action == toggle && isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace []
         tileWin win

  -- It shouldn't be necessary for xmonad to do anything more with this event
  return $ All False

evHook _ = return $ All True
-- End helper functions for fullscreen


main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ defaultConfig
       { manageHook = manageDocks <+> manageHook defaultConfig <+> myManageHook
       , layoutHook = myLayoutHook
       , handleEventHook = evHook
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
