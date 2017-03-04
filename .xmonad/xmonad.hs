import XMonad

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook

import XMonad.StackSet (swapUp, swapDown, shiftMaster)

import XMonad.Util.Run(spawnPipe)

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace 
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.Named
import XMonad.Layout.Circle
import XMonad.Layout.Gaps

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Man

import XMonad.Util.Scratchpad

import qualified XMonad.StackSet as S

import System.IO
import Data.Ratio ((%))
import qualified Data.Map as M

import Data.List (isPrefixOf)


wmName       = stringProperty "WM_NAME"
wmWindowRole = stringProperty "WM_WINDOW_ROLE"

floatClass = []
floatTitle = []
hacking    = ["Happy Hacking"]
coding     = ["Happy Coding"]
webApps    = ["Firefox", "Google-chrome", "Chromium"]
comApps    = ["Pidgin", "jabber", "Jabber", "Empathy"]
mailApps   = ["OUTLOOK.EXE", "Wine", "mutt", "mail", "evolution", "Evolution"]
gimpApp    = ["Gimp", "gimp"]
skypeApp   = ["Skype", "skype", "MainWindow"]
ircApps    = ["workies", "irc"]

wmSkypeMain x = do x1 <- className    =? x
                   x2 <- wmWindowRole =? "MainWindow"
                   return (x1 && x2)

myWorkspaces = map show [1] ++ ["code", "web", "im", "mail", "skype", "gimp", "musi", "irc"]

curLayout :: X String
curLayout = gets windowset >>= return . description . S.layout . S.workspace . S.current

main = do
         spawnPipe "/home/frosch03/bin/xStartup"
         xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
            { borderWidth        = 1
            , normalBorderColor  = "#333333"
            , focusedBorderColor = "#999999"
            , modMask            = mod4Mask
            , keys               = newKeys
            , workspaces         = myWorkspaces
            , manageHook         = manageDocks <+> manageHook defaultConfig <+> myManagedHook
            , layoutHook         = avoidStruts $ myLayout
            , logHook            = dynamicLogWithPP $ myLemonbarPP
            } 

-- The pretty printed layout for my lemonbar 
myLemonbarPP = defaultPP 
                 { ppTitle   =  ("WIN" ++)
                 , ppCurrent =  ("FOC" ++)
                 , ppVisible =  ("ACT" ++)
                 , ppUrgent  =  ("URG" ++)
                 , ppHidden  =  ("INA" ++)
		 , ppLayout  =  ("LAY" ++)
                 , ppSep     = "\n"
		 , ppWsSep   = " "
		 , ppOutput  = \x -> writeFile "/tmp/xmonad_lemonbar_frosch03" $ "WSP" ++ x ++ "\n"
                 } 
                 

-- The layoutdefinition for my workspaces
myLayout  = smartBorders
          $ onWorkspaces ["1"]      (Full ||| tiled ||| Mirror tiled)
          $ onWorkspaces ["code"]   (codeColumn ||| codeRow)
          $ onWorkspaces ["web"]    (Full ||| (gaps [(L,250), (R,250)] $ Full))
          -- $ onWorkspaces ["im"]     (Full)
          $ onWorkspaces ["im"]     (   reflectHoriz (pidginLayout gridLayout)
                                    ||| reflectHoriz (pidginLayout Full)
                                    ||| reflectHoriz (pidginLayout Circle)
                                    )
	  $ onWorkspaces ["mail"]   (Full)
	  $ onWorkspaces ["skype"]  (reflectHoriz $ (withIM (1%6) (Role "MainWindow") Grid))
 	  $ onWorkspaces ["gimp"]   (gimp)
       	  $ onWorkspaces ["musi"]   (Circle)
	  $ tiled ||| Mirror tiled ||| Full ||| Circle
    where tiled   = Tall nmaster delta ratio
          nmaster = 1 
          ratio   = 1/2
          delta   = 3/100
          codeColumn = named "Column Code" $ Tall nmaster delta' ratio'
          codeRow    = named "Row Code" $ Mirror $ reflectHoriz $ Tall nmaster delta' ratio'
          ratio'     = 1/3
          delta'     = 0
          gridLayout = spacing 8 $ Grid      
          pidginLayout l = withIM (18/100) (Role "buddy_list") l
          gimp       = withIM (0.11) (Role "gimp-toolbox") $
                       reflectHoriz $
                       withIM (0.15) (Role "gimp-dock") Full

-- The XPConfig definition for my XMonad.Prompts 
myXPConfig = defaultXPConfig { bgColor     = "black"
                             , fgColor     = "white"
                             , borderColor = "#333333"
                             , position    = Top
                             , font        = "xft:Inconsolata:pixelsize=10:antialias=true:autohint=true"
                             }

xK_XF86Play = 0x1008FF14
xK_XF86Stop = 0x1008FF15
xK_XF86Fwrd = 0x1008FF17
xK_XF86Bwrd = 0x1008FF16
xK_XF86Thnk = 0x1008FF41
xK_XF86Sleep            = 0x1008ff2f
xK_XF86ScreenSaver      = 0x1008ff2d
xK_XF86AudioRaiseVolume = 0x1008ff13
xK_XF86AudioLowerVolume = 0x1008ff11
xK_FroggersPause = 0x1008ff12


-- My additional keybindings
myKeys x = M.fromList $
  [ ((modMask x,                 xK_p), shellPrompt myXPConfig)
  , ((modMask x .|. shiftMask,   xK_r), spawn "/usr/bin/urxvt -e emacsclient -q -nw -e '(remember)'")
  , ((modMask x .|. shiftMask,   xK_e), spawn "/usr/bin/urxvt -e emacsclient -q -nw -e '(eshell)'")
  , ((modMask x,                 xK_y), scratchpadSpawnActionTerminal "urxvt")
  , ((modMask x,                 xK_m), spawn "/usr/bin/dmpc")
  , ((modMask x .|. shiftMask,   xK_m), manPrompt myXPConfig)
  , ((modMask x .|. shiftMask,   xK_s), sshPrompt myXPConfig)
  , ((modMask x .|. shiftMask,   xK_f), focusUrgent)
  , ((modMask x .|. shiftMask,   xK_j), windows swapDown)
  , ((modMask x .|. shiftMask,   xK_k), windows swapUp)
  , ((modMask x,                 xK_m), windows shiftMaster)
  , ((modMask x .|. shiftMask,   xK_Return), spawn "/usr/bin/urxvt")
  , ((modMask x .|. controlMask, xK_l),	spawn "/home/frosch03/bin/lock")
  , ((0        ,          xK_XF86Play), spawn "/usr/bin/mpc toggle")
  , ((0        ,          xK_XF86Stop), spawn "/usr/bin/mpc stop")
  , ((0        ,          xK_XF86Fwrd), spawn "/usr/bin/mpc next")
  , ((0        ,          xK_XF86Bwrd), spawn "/usr/bin/mpc prev")
  , ((0        ,          xK_XF86Thnk), spawn "/home/frosch03/whatIsThisPlace")
  , ((0        ,         xK_XF86Sleep),	spawn "sudo pm-suspend")
  , ((0        ,   xK_XF86ScreenSaver),	spawn "gnome-screensaver-command --lock")
  ]
newKeys x = myKeys x `M.union` keys defaultConfig x

-- My additional managed applications (browser is always on desktop 3 and in fullscreen, etc.)
myManagedHook = composeAll . concat $
  [ [ className =? c --> doFloat               | c <- floatClass ]
  , [ title     =? t --> doFloat               | t <- floatTitle ]
  , [ title     =? x --> doF (S.shift "1")     | x <- hacking]
  , [ title     =? x --> doF (S.shift "code")  | x <- coding]
  , [ className =? x --> doF (S.shift "web")   | x <- webApps ]
  , [ className =? x --> doF (S.shift "im")    | x <- comApps ]
  , [ title     =? x --> doF (S.shift "im")    | x <- comApps ]
  , [ className =? x --> doF (S.shift "mail")  | x <- mailApps ]
  , [ title     =? x --> doF (S.shift "mail")  | x <- mailApps ]
  , [ className =? x --> doF (S.shift "gimp")  | x <- gimpApp ]
  , [ className =? x --> doF (S.shift "skype") | x <- skypeApp ]
  , [ title     =? x --> doF (S.shift "irc")   | x <- ircApps ]
  , [ isFullscreen   --> doFullFloat]
  ]
