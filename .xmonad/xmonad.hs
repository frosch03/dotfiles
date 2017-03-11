import XMonad

import XMonad.Actions.ConditionalKeys       -- bindings per workspace or layout (for bindOn)
import XMonad.Actions.CopyWindow            -- like cylons, except x windows
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.MessageFeedback       -- pseudo conditional key bindings
import XMonad.Actions.Navigation2D          -- (for windowGo)
import XMonad.Actions.Promote               -- promote window to master
import XMonad.Actions.SpawnOn
import XMonad.Actions.WithAll               -- action all the things

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.ComboP                 -- SwapWindow
import XMonad.Layout.Hidden                 -- for popOldestHiddenWindow
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.

import qualified XMonad.StackSet as W       -- myManageHookShift

import XMonad.Prompt                        -- to get my old key bindings working
import XMonad.Prompt.ConfirmPrompt          -- don't just hard quit

import XMonad.Util.EZConfig                 -- removeKeys, additionalKeys
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste as P               -- testing
import XMonad.Util.Run                      -- for spawnPipe and hPutStrLn
import XMonad.Util.WorkspaceCompare         -- custom WS functions filtering NSP
import XMonad.Util.XSelection
    

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

import Control.Monad (liftM, liftM2, join)  -- myManageHookShift
import System.Exit
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

wsOne   = "one"
wsCode  = "code"
wsWeb   = "web"
wsIm    = "im"
wsMail  = "mail"
wsSkype = "skype"
wsGimp  = "gimp"
wsMusi  = "musi"
wsIrc   = "irc"

-- myWorkspaces = map show [1] ++ ["code", "web", "im", "mail", "skype", "gimp", "musi", "irc"]
myWorkspaces = [wsOne, wsCode, wsWeb, wsIm, wsMail, wsSkype, wsGimp, wsMusi, wsIrc]



projects :: [Project]
projects =

    [ Project   { projectName       = "code"
                , projectDirectory  = "~/Programming/"
                , projectStartHook  = Just $ do spawnOn "code" myTerminal
                                                spawnOn "code" myTerminal
                }
    ]




curLayout :: X String
curLayout = gets windowset >>= return . description . S.layout . S.workspace . S.current

main = do
  spawnPipe "/home/frosch03/bin/xStartup"
  xmonad
       $ dynamicProjects projects
       $ withUrgencyHook NoUrgencyHook
       $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys'
       $ defaultConfig
             { borderWidth        = 1
             , normalBorderColor  = "#333333"
             , focusedBorderColor = "#999999"
             , modMask            = myModMask
             -- , keys               = newKeys
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


myTerminal = "/usr/bin/urxvt"
myBrowser  = "/usr/bin/firefox"

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
  , ((modMask x .|. shiftMask,   xK_Return), spawn myTerminal)
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


myModMask = mod4Mask

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

wsKeys = map show $ [1..9] ++ [0]
            

myKeys' conf = let

    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    dirKeys        = [ "j",  "k",  "h",  "l"]
    arrowKeys      = ["<D>","<U>","<L>","<R>"]
    dirs           = [  D,    U,    L,    R ]

    --screenAction f        = screenWorkspace >=> flip whenJust (windows . f)

    zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
    zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

    -- from xmonad.layout.sublayouts
    focusMaster' st = let (f:fs) = W.integrate st
        in W.Stack f [] fs
    swapMaster' (W.Stack f u d) = W.Stack f [] $ reverse u ++ d

    -- try sending one message, fallback if unreceived, then refresh
    tryMsgR x y = sequence_ [(tryMessage_ x y), refresh]

    -- warpCursor = warpToWindow (9/10) (9/10)

    -- cf https://github.com/pjones/xmonadrc
    --switch :: ProjectTable -> ProjectName -> X ()
    --switch ps name = case Map.lookup name ps of
    --  Just p              -> switchProject p
    --  Nothing | null name -> return ()

    -- do something with current X selection
    unsafeWithSelection app = join $ io $ liftM unsafeSpawn $ fmap (\x -> app ++ " " ++ x) getSelection

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

    in

    -----------------------------------------------------------------------
    -- Launchers
    -----------------------------------------------------------------------
    subKeys "Launchers"
    [ -- ("M-<Space>"              , addName "Launcher"                        $ spawn myLauncher)
    -- , 
      ("M-<Return>"             , addName "Terminal"                        $ spawn myTerminal)
    , ("M-\\"                   , addName "Browser"                         $ spawn myBrowser)
    -- , ("M-c"                    , addName "NSP Chat"                        $ bindOn WS [(wsWRK, namedScratchpadAction scratchpads "hangoutsWork"),
    --                                                                           ("", namedScratchpadAction scratchpads "hangoutsPersonal")])
    -- , ("M-t"                    , addName "NSP Tasks"                       $ bindOn WS [(wsWRK, namedScratchpadAction scratchpads "trelloWork"),
    --                                                                           ("", namedScratchpadAction scratchpads "trello")])
    -- , ("M-m"                    , addName "NSP Music"                       $ namedScratchpadAction scratchpads "googleMusic")
    -- , ("M-v"                    , addName "NSP Video"                       $ namedScratchpadAction scratchpads "plex")
    -- , ("M1-x"                   , addName "NSP Xawtv"                       $ namedScratchpadAction scratchpads "xawtv")
    -- , ("M-n"                    , addName "NSP Console"                     $ namedScratchpadAction scratchpads "console")
    , ("M-s s"                  , addName "Cancel submap"                   $ return ())
    , ("M-s M-s"                , addName "Cancel submap"                   $ return ())
    ] ^++^

    -----------------------------------------------------------------------
    -- Windows
    -----------------------------------------------------------------------

    subKeys "Windows"
    (
    [
      ("M-<Backspace>"          , addName "Kill"                            kill1)
    , ("M-S-<Backspace>"        , addName "Kill all"                        $ confirmPrompt hotPromptTheme "kill all" $ killAll)
    , ("M-d"                    , addName "Duplicate w to all ws"           $ windows copyToAll)
    , ("M-S-d"                  , addName "Kill other duplicates"           $ killAllOtherCopies)
    , ("M-d"                    , addName "Duplicate w to all ws"           $ toggleCopyToAll)
    , ("M-p"                    , addName "Hide window to stack"            $ withFocused hideWindow)
    , ("M-S-p"                  , addName "Restore hidden window (FIFO)"    $ popOldestHiddenWindow)

    , ("M-b"                    , addName "Promote"                         $ promote) 

    , ("M-g"                    , addName "Un-merge from sublayout"         $ withFocused (sendMessage . UnMerge))
    , ("M-S-g"                  , addName "Merge all into sublayout"        $ withFocused (sendMessage . MergeAll))

    , ("M-z u"                  , addName "Focus urgent"                    focusUrgent)
    , ("M-z m"                  , addName "Focus master"                    $ windows W.focusMaster)

    --, ("M-<Tab>"              	, addName "Focus down"                      $ windows W.focusDown)
    --, ("M-S-<Tab>"              , addName "Focus up"                        $ windows W.focusUp)

    , ("M-'"                    , addName "Cycle current tabs D"            $ bindOn LD [("Tabs", windows W.focusDown), ("", onGroup W.focusDown')])
    , ("M-;"                    , addName "Cycle current tabs U"            $ bindOn LD [("Tabs", windows W.focusUp), ("", onGroup W.focusUp')])

    -- ComboP specific (can remove after demo)
    , ("M-C-S-m"                , addName "Combo swap"                      $ sendMessage $ SwapWindow)
    ]

    ++ zipM' "M-"               "Navigate window"                           dirKeys dirs windowGo True
    -- ++ zipM' "M-S-"               "Move window"                               dirKeys dirs windowSwap True
    -- TODO: following may necessitate use of a "passthrough" binding that can send C- values to focused w
    ++ zipM' "M-C-"             "Move window"                               dirKeys dirs windowSwap True
    -- ++ zipM  "M-C-"             "Merge w/sublayout"                         dirKeys dirs (sendMessage . pullGroup)
    ++ zipM' "M-"               "Navigate screen"                           arrowKeys dirs screenGo True
    -- ++ zipM' "M-S-"             "Move window to screen"                     arrowKeys dirs windowToScreen True
    ++ zipM' "M-C-"             "Move window to screen"                     arrowKeys dirs windowToScreen True
    ++ zipM' "M-S-"             "Swap workspace to screen"                  arrowKeys dirs screenSwap True

    ) ^++^

    -----------------------------------------------------------------------
    -- Workspaces & Projects
    -----------------------------------------------------------------------

    -- original version was for dynamic workspaces
    --    subKeys "{a,o,e,u,i,d,...} focus and move window between workspaces"
    --    (  zipMod "View      ws" wsKeys [0..] "M-"      (withNthWorkspace W.greedyView)

    subKeys "Workspaces & Projects"
    (
    [ -- ("M-w"                    , addName "Switch to Project"           $ switchProjectPrompt warmPromptTheme)
    -- , ("M-S-w"                  , addName "Shift to Project"            $ shiftToProjectPrompt warmPromptTheme)
    -- , 
      ("M-<Escape>"             , addName "Next non-empty workspace"    $ nextNonEmptyWS)
    , ("M-S-<Escape>"           , addName "Prev non-empty workspace"    $ prevNonEmptyWS)
    , ("M-`"                    , addName "Next non-empty workspace"    $ nextNonEmptyWS)
    , ("M-S-`"                  , addName "Prev non-empty workspace"    $ prevNonEmptyWS)
    , ("M-a"                    , addName "Toggle last workspace"       $ toggleWS' ["NSP"])
    ]
    ++ zipM "M-"                "View      ws"                          wsKeys [0..] (withNthWorkspace W.greedyView)
    ++ zipM "M-S-"              "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    -- TODO: following may necessitate use of a "passthrough" binding that can send C- values to focused w
    -- ++ zipM "C-"                "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    -- TODO: make following a submap
    ++ zipM "M-S-C-"            "Copy w to ws"                          wsKeys [0..] (withNthWorkspace copy)
    ) ^++^

    -----------------------------------------------------------------------
    -- Layouts & Sublayouts
    -----------------------------------------------------------------------

    subKeys "Layout Management"

    [ ("M-<Tab>"                , addName "Cycle all layouts"               $ sendMessage NextLayout)
    , ("M-C-<Tab>"              , addName "Cycle sublayout"                 $ toSubl NextLayout)
    , ("M-S-<Tab>"              , addName "Reset layout"                    $ setLayout $ XMonad.layoutHook conf)

    , ("M-y"                    , addName "Float tiled w"                   $ withFocused toggleFloat)
    , ("M-S-y"                  , addName "Tile all floating w"             $ sinkAll)

    , ("M-r"                    , addName "Reflect/Rotate"              $ tryMsgR (Rotate) (XMonad.Layout.MultiToggle.Toggle REFLECTX))
    , ("M-S-r"                  , addName "Force Reflect (even on BSP)" $ sendMessage (XMonad.Layout.MultiToggle.Toggle REFLECTX))

    -- If following is run on a floating window, the sequence first tiles it.
    -- Not perfect, but works.
    , ("M-f"                , addName "Fullscreen"                      $ sequence_ [ (withFocused $ windows . W.sink)
                                                                        , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) ])

    -- Fake fullscreen fullscreens into the window rect. The expand/shrink
    -- is a hack to make the full screen paint into the rect properly.
    -- The tryMsgR handles the BSP vs standard resizing functions.
    , ("M-S-f"                  , addName "Fake fullscreen"             $ sequence_ [ (P.sendKey P.noModMask xK_F11)
                                                                                    , (tryMsgR (ExpandTowards L) (Shrink))
                                                                                    , (tryMsgR (ExpandTowards R) (Expand)) ])
    ] ^++^

    -----------------------------------------------------------------------
    -- System / Utilities
    -----------------------------------------------------------------------
    subKeys "System"
    [ ("M-q"          , addName "Restart XMonad"            $ spawn "xmonad --restart")
    , ("M-i"          , addName "Run a programm"            $ shellPrompt myXPConfig)
    -- , ("M-S-r"        , addName "Emacs remember window"     $ spawn "/usr/bin/urxvt -e emacsclient -q -nw -e '(remember)'")
    -- , ("M-S-e"        , addName "Emacs eshell"              $ spawn "/usr/bin/urxvt -e emacsclient -q -nw -e '(eshell)'")
    -- , ("M-y"          , addName "Scratchpad shell"          $ scratchpadSpawnActionTerminal "urxvt")
    -- , ("M-S-m"        , addName "" $ manPrompt myXPConfig)
    -- , ("M-S-s"        , addName "" $ sshPrompt myXPConfig)
    -- , ("M-S-f"        , addName "" $ focusUrgent)
    -- , ("M-S-j"        , addName "" $ windows swapDown)
    -- , ("M-S-k"        , addName "" $ windows swapUp)
    -- , ("M-m"          , addName "" $ windows shiftMaster)
    -- , ("M-S-<Return>" , addName "" $ spawn myTerminal)
    -- , ("M-S-l"        ,	addName "" $ spawn "/home/frosch03/bin/lock")

    , ("M-C-q"        , addName "Rebuild & restart XMonad"  $ spawn "xmonad --recompile && xmonad --restart")
    -- , ("M-S-q"        , addName "Quit XMonad"               $ confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))
    -- , ("M-x"          , addName "Lock screen"               $ spawn "xset s activate")
    -- , ("M-<F4>"       , addName "Print Screen"              $ return ())
  --, ("M-F1"                   , addName "Show Keybindings"                $ return ())
    ] -- ^++^
              where
		toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
							         [] -> windows copyToAll
				                                 _  -> killAllOtherCopies



base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"


hotPromptTheme = myPromptTheme
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
    }

myPromptTheme = def
    { bgColor               = base03
    , fgColor               = blue
    , fgHLight              = base03
    , bgHLight              = blue
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = 20
    , position              = Top
    }
