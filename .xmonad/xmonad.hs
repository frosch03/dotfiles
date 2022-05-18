{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
import XMonad

-- import XMonad.Actions.ConditionalKeys       -- bindings per workspace or layout (for bindOn)
import XMonad.Actions.CopyWindow            -- like cylons, except x windows
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.MessageFeedback       -- pseudo conditional key bindings
import XMonad.Actions.Navigation2D          -- (for windowGo)
import XMonad.Actions.Promote               -- promote window to master
import XMonad.Actions.SpawnOn
import XMonad.Actions.WithAll               -- action all the things

import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.ComboP                 -- SwapWindow
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import XMonad.Layout.Hidden                 -- for popOldestHiddenWindow
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration     -- Have window deco topbar the whole window size
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile          -- Resizable Horizontal border
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

import qualified XMonad.StackSet as W       -- myManageHookShift

--import Data.Default
import XMonad.Prompt                        -- to get my old key bindings working
import XMonad.Prompt.ConfirmPrompt          -- don't just hard quit
import XMonad.Prompt.Input                  -- for the capture window

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

import XMonad.Config.Desktop


import qualified XMonad.StackSet as S

import Control.Monad (liftM, liftM2, join)  -- myManageHookShift
import System.Exit
import System.IO
import Data.Ratio ((%))
import qualified Data.Map as M

import Data.List (isPrefixOf)


wmName       = stringProperty "WM_NAME"
wmWindowRole = stringProperty "WM_WINDOW_ROLE"

floatClass  = []
floatTitle  = []
hacking     = ["Happy Hacking"]
configuring = ["Happy Configuring"]
webApps     = ["Firefox", "Google-chrome", "Chromium"]
comApps     = ["Pidgin", "jabber", "Jabber", "Empathy"]
mailApps    = ["OUTLOOK.EXE", "Wine", "mutt", "mail", "evolution", "Evolution"]
gimpApp     = ["Gimp", "gimp"]
skypeApp    = []
ircApps     = []

wmSkypeMain x = do x1 <- className    =? x
                   x2 <- wmWindowRole =? "MainWindow"
                   return (x1 && x2)

wsOne   = "HACKING"
wsCode  = "CONFIGURING"
wsWeb   = "BROWSING"
wsComm  = "COMM"
wsMail  = "MAIL"
wsSkype = "SIX"
wsGimp  = "SEVEN"
wsMusi  = "EIGHT"
wsIrc   = "NINE"


myWorkspaces = [wsOne, wsCode, wsWeb, wsComm, wsMail, wsSkype, wsGimp, wsMusi, wsIrc]



projects :: [Project]
projects =

    [ Project   { projectName       = wsOne
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsOne myTmuxTerminal
                }

    , Project   { projectName       = wsCode
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsCode myTmuxTerminal
                                                spawnOn wsCode "xclock"
                                                spawnOn wsCode "pavucontrol"
                                                spawnOn wsCode "signal-desktop"
                }

    , Project   { projectName       = wsWeb
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsWeb myBrowser
                                                spawnOn wsWeb "emacsclient -c -F '(quote (name . \"Notizen\"))' ~/Dropbox/Apps/SimpleTxtEditor/Notizen.org"
                }

    , Project   { projectName       = wsComm
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsComm "emacsclient -q -c -e '(start-irc)'"
                }

    , Project   { projectName       = wsMail
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsMail "emacsclient -q -c -e '(mu4e)'"
                }

    ]




curLayout :: X String
curLayout = gets windowset >>= return . description . S.layout . S.workspace . S.current

main = do
  spawn "/home/frosch03/bin/xStartup &"
  xmonad
       -- $ ewmh
       $ dynamicProjects projects
       $ withUrgencyHook NoUrgencyHook
       $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys'
       $ desktopConfig
             { borderWidth        = 0
             , normalBorderColor  = myGray
             , focusedBorderColor = "#999999"
             , modMask            = myModMask
             , workspaces         = myWorkspaces
             -- , handleEventHook    = myEventHook <+> fullscreenEventHook
             , manageHook         = myManagedHook
             , layoutHook         = myLayoutHook
             , logHook            = (dynamicLogWithPP $ myLemonbarPP) <+> myLogHook
             } 
 
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
     where fadeAmount = 0xcccccccc
     
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
                 
-- The XPConfig definition for my XMonad.Prompts 
myXPConfig = defaultXPConfig { bgColor     = "#000030" -- dark dark blue
                             , fgColor     = "white"
                             , borderColor = "#000060"
                             , position    = Top
                             -- , font        = "xft:Inconsolata Nerd Font Mono-11:style=Regular"
                             , font        = "xft:PragmataPro Mono-8:style=Regular"
                             }


-- The layoutdefinition for my workspaces
-- myLayout  = smartBorders
--           $ onWorkspaces ["1"]      (Full ||| tiled ||| Mirror tiled)
--           $ onWorkspaces ["code"]   (codeColumn ||| codeRow)
--           $ onWorkspaces ["web"]    (Full ||| (gaps [(L,250), (R,250)] $ Full))
--           -- $ onWorkspaces ["im"]     (Full)
--           $ onWorkspaces ["im"]     (   reflectHoriz (pidginLayout gridLayout)
--                                     ||| reflectHoriz (pidginLayout Full)
--                                     ||| reflectHoriz (pidginLayout Circle)
--                                     )
-- 	  $ onWorkspaces ["mail"]   (Full)
-- 	  $ onWorkspaces ["skype"]  (reflectHoriz $ (withIM (1%6) (Role "MainWindow") Grid))
--  	  $ onWorkspaces ["gimp"]   (gimp)
--        	  $ onWorkspaces ["musi"]   (Circle)
-- 	  $ tiled ||| Mirror tiled ||| Full ||| Circle
--     where tiled   = Tall nmaster delta ratio
--           nmaster = 1 
--           ratio   = 1/2
--           delta   = 3/100
--           codeColumn = named "Column Code" $ Tall nmaster delta' ratio'
--           codeRow    = named "Row Code" $ Mirror $ reflectHoriz $ Tall nmaster delta' ratio'
--           ratio'     = 1/3
--           delta'     = 0
--           gridLayout = spacing 8 $ Grid      
--           pidginLayout l = withIM (18/100) (Role "buddy_list") l
--           gimp       = withIM (0.11) (Role "gimp-toolbox") $
--                        reflectHoriz $
--                        withIM (0.15) (Role "gimp-dock") Full


data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)

-- tabBarFull = avoidStruts $ noFrillsDeco shrinkText topBarTheme $ addTabs shrinkText myTabTheme $ Simplest
barFull = avoidStruts $ Simplest

myLayoutHook = hiddenWindows
             $ avoidStruts $ showWorkspaceName
             -- $ onWorkspace "AV" floatWorkSpace
             $ fullscreenFloat -- fixes floating windows going full screen, while retaining "bounded" fullscreen
             $ fullScreenToggle
             $ fullBarToggle
             $ mirrorToggle
             $ reflectToggle
             $ threeCol ||| flex ||| tabs
  where

--    testTall = Tall 1 (1/50) (2/3)
--    myTall = subLayout [] Simplest $ trackFloating (Tall 1 (1/20) (1/2))

    -- floatWorkSpace      = simplestFloat
    fullBarToggle       = mkToggle (single FULLBAR)
    fullScreenToggle    = mkToggle (single FULL)
    mirrorToggle        = mkToggle (single MIRROR)
    reflectToggle       = mkToggle (single REFLECTX)
    smallMonResWidth    = 1920
    showWorkspaceName   = showWName' myShowWNameTheme

    named n             = renamed [(XMonad.Layout.Renamed.Replace n)]
    trimNamed w n       = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                                   (XMonad.Layout.Renamed.PrependWords n)]
    suffixed n          = renamed [(XMonad.Layout.Renamed.AppendWords n)]
    trimSuffixed w n    = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                                   (XMonad.Layout.Renamed.AppendWords n)]

    addTopBar           = noFrillsDeco shrinkText topBarTheme

    mySpacing           = spacing gap
    sGap                = quot gap 2
    myGaps              = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
    mySmallGaps         = gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]
    myBigGaps           = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]

    --------------------------------------------------------------------------
    -- Tabs Layout                                                          --
    --------------------------------------------------------------------------

    -- threeCol = named "Unflexed"
    --      $ avoidStruts
    --      $ addTopBar
    --      $ myGaps
    --      $ mySpacing
    --      $ ThreeColMid 1 (1/10) (1/2)
                          
    tabs = named "Tabs"
         -- $ avoidStruts
         $ addTopBar
         $ addTabs shrinkText myTabTheme
         $ Simplest

    -----------------------------------------------------------------------
    -- Flexi SubLayouts                                                  --
    -----------------------------------------------------------------------
    --
    -- In many ways the best solution. Acts like ThreeColumns, Tall, BSP,
    -- or any other container layout style. Can use this layout just as you
    -- would those without tabs at all, or you can easily merge any windows
    -- into a tabbed group.
    --
    -- Diagrams:
    --
    -- (examples only... this is a very flexible layout and as such the
    -- layout style and arrangement isn't limited as much as the other
    -- attempts below)
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |   Tabs   |
    -- |          |                    |          |
    -- |----------|       Master       |----------|
    -- |          |                    |          |
    -- |   Tabs   |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |----------|
    -- |                    |          |
    -- |                    |   Tabs   |
    -- |                    |          |
    -- ---------------------------------
    --
    --
    -- Advantages
    --
    --   * tab group is movable as a unit and acts like any other window
    --
    --   * this is the "cleanest" of the dynamic layouts I've worked with
    --     and leaves no "pixel dust" on the screen when switching to a WS
    --     on a different monitor
    --
    --   * navigation and window/group movement is trivial with
    --     X.A.Navigation2D
    --
    --   * master window remains master when switching screens (unlike
    --     the "X.L.Master" based solution below)
    --
    --   * unlike some of the other solutions, it is trivial to change
    --     the exterior layout format and so I could potentially add in
    --     some layout change to BSP or other layout that I want to test
    --     while still retaining the tab functionality
    --
    -- Disadvantages
    --
    --   * layout starts without any tabs (could be considered a feature
    --     since in that case the layout performs exactly as the parent or
    --     container layout does)
    --
    --   * To move a window into or out of the tabbed group requires
    --     special key bindings unique to X.L.SubLayouts
    --
    --  Understanding XMonad.Layouts.SubLayouts
    --
    --  It took me a while to grok this.
    --
    --  the subLayout hook is used with the following format:
    --
    --    subLayout advanceInnerLayouts innerLayout outerLayout
    --
    --  It works like this: subLayout modifies an entire other layout (or
    --  layouts), enabling you to turn what would be a normal window into
    --  a little group of windows managed by an entirely different layout.
    --
    --  In my case, I'm using layouts like "Three Column" and "Tall" as the
    --  nominal "container" layout (what SubLayouts calls the "outerLayout").
    --
    --  The "inner layout" in my case is just "Simplest". I'm also adding tabs
    --  which are only applied to my sublayouts. Not sure how that works
    --  but it's apparent from the X.L.SubLayouts documentation that this is
    --  the intended use/behavior. Essential X.L.SubLayouts is hijacking these
    --  added tabs and applying them just to the Simplest layout, and then that
    --  in turn is stuck inside the rectangle that would normally hold a window
    --  in my normal layouts.
    --
    --  One of the confusing things for me at first was that the layout doesn't
    --  start with any subLayouts. So it appears to just be a normal layout.
    --  You have to "merge all" to suck everything up into a Simplest tabbed
    --  group and then you can add other windows normally and you'll
    --  have a sublayout with tabs.
    --
    --  Note: subLayouts has some other features. For example, you can give it
    --  a list of layouts to work through and it will advance through them in
    --  series (or possibly in an order your provide) and will apply different
    --  layouts to different subLayout groups. Each time you add a new window
    --  to your layout, it acquires the sublayout, even if you don't know it.
    --
    --  In my case, my list is one long and is just the first window I add.
    --
    --  Ex. The second group is Tall, the third is Circle, all others are
    --  tabbed with:
    --
    --  myLayout = addTabs shrinkText def
    --           $ subLayout [0,1,2] (Simplest ||| Tall 1 0.2 0.5 ||| Circle)
    --                    $ Tall 1 0.2 0.5 ||| Full
    -- 
    -- this is a flexible sublayout layout that has only one container
    -- layout style (depending on screen)
    --     flexiSub = named "Flexi SubLayouts"
    --               $ avoidStruts
    --               $ windowNavigation
    --               $ addTopBar
    --               $ myGaps
    --               $ addTabs shrinkText myTabTheme
    --               $ mySpacing
    --               $ subLayout [] Simplest
    --               $ ifWider smallMonResWidth wideLayout standardLayout
    --               where
    --                   wideLayout = ThreeColMid 1 (1/100) (1/2)
    --                   standardLayout = ResizableTall 1 (1/50) (2/3) []
    --
    -- retained during development: safe to remove later

    threeCol =
        trimNamed 5 "3 Col" $
                  windowNavigation $ addTopBar $ myGaps $ mySpacing $ ThreeCol 1 (3 / 100) (1 / 2)

    flex = trimNamed 5 "Flex"
              -- $ avoidStruts
              -- don't forget: even though we are using X.A.Navigation2D
              -- we need windowNavigation for merging to sublayouts
              $ windowNavigation
              $ addTopBar
	      $ myGaps
              $ addTabs shrinkText myTabTheme
	      $ mySpacing
              -- $ subLayout [] (Simplest ||| (mySpacing $ Accordion))
              $ subLayout [] (Simplest ||| Accordion)
              $ ifWider smallMonResWidth wideLayouts standardLayouts
              where
                  wideLayouts = myGaps $ mySpacing
                      $ (suffixed "Wide 3Col" $ ThreeColMid 1 (1/20) (1/2))
                    ||| (trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP)
                  --  ||| fullTabs
                  standardLayouts = myGaps $ mySpacing
                      $ (suffixed "Std 2/3" $ ResizableTall 1 (1/20) (2/3) [])
                    ||| (suffixed "Std 1/2" $ ResizableTall 1 (1/20) (1/2) [])

                  --  ||| fullTabs
                  --fullTabs = suffixed "Tabs Full" $ Simplest
                  --
                  -- NOTE: removed this from the two (wide/std) sublayout
                  -- sequences. if inside the ifWider, the ||| combinator
                  -- from X.L.LayoutCombinators can't jump to it directly (
                  -- or I'm doing something wrong, either way, it's simpler
                  -- to solve it by just using a tabbed layout in the main
                  -- layoutHook). The disadvantage is that I lose the "per
                  -- screen" memory of which layout was where if using the
                  -- tabbed layout (if using the the ifWider construct as
                  -- I am currently, it seems to work fine)
                  --
                  -- Using "Full" here (instead of Simplest) will retain the
                  -- tabbed sublayout structure and allow paging through each
                  -- group/window in full screen mode. However my preference
                  -- is to just see all the windows as tabs immediately.  
                  -- Using "Simplest" here will do this: display all windows
                  -- as tabs across the top, no "paging" required. However
                  -- this is misleading as the sublayouts are of course still
                  -- there and you will have to use the nornmal W.focusUp/Down
                  -- to successfully flip through them. Despite this
                  -- limitation I prefer this to the results with "Full".

{-|
    -----------------------------------------------------------------------
    -- Simple Flexi                                                      --
    -----------------------------------------------------------------------
    --
    -- Simple dynamically resizing layout as with the other variations in
    -- this config. This layout has not tabs in it and simply uses
    -- Resizable Tall and Three Column layouts.

    simpleFlexi = named "Simple Flexible"
              $ ifWider smallMonResWidth simpleThree simpleTall

    simpleTall = named "Tall"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ ResizableTall 1 (1/300) (2/3) []
              
    simpleThree = named "Three Col"
              $ avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ ThreeColMid 1 (3/100) (1/2)

    -----------------------------------------------------------------------
    -- Other Misc Layouts                                                --
    -----------------------------------------------------------------------
    --
    --

    masterTabbedP   = named "MASTER TABBED"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ mastered (1/100) (1/2) $ tabbed shrinkText myTabTheme

    bsp       = named "BSP"
              $ borderResize (avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ emptyBSP )
              -- $ borderResize (emptyBSP)

    oneBig    = named "1BG"
              $ avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ OneBig (3/4) (3/4)

    tiledP    = named "TILED"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ consoleOn
              $ tiled'

    oneUp =   named "1UP"
              $ avoidStruts
              $ myGaps
              $ combineTwoP (ThreeCol 1 (3/100) (1/2))
                            (Simplest)
                            (Tall 1 0.03 0.5)
                            (ClassName "Google-chrome-beta")

    -----------------------------------------------------------------------
    -- Master-Tabbed Dymamic                                             --
    -----------------------------------------------------------------------
    --
    -- Dynamic 3 pane layout with one tabbed panel using X.L.Master
    -- advantage is that it can do a nice 3-up on both ultrawide and
    -- standard (laptop in my case) screen sizes, where the layouts
    -- look like this:
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |  Master  |       Master       |   Tabs   |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    -- \____________________ _____________________/
    --                      '
    --                 all one layout
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |   Tabs   |
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- ---------------------------------
    -- \_______________ _______________/
    --                 '
    --            all one layout
    --
    -- Advantages to this use of X.L.Master to created this dynamic
    -- layout include:
    --
    --   * No fussing with special keys to swap windows between the
    --     Tabs and Master zones
    --
    --   * Window movement and resizing is very straightforward
    --
    --   * Limited need to maintain a mental-map of the layout
    --     (pretty easy to understand... it's just a layout)
    --
    -- Disadvantages include:
    --
    --   * Swapping a window from tabbed area will of necessity swap
    --     one of the Master windows back into tabs (since there can
    --     only be two master windows)
    --
    --   * Master area can have only one/two windows in std/wide modes
    --     respectively
    --
    --   * When switching from wide to standard, the leftmost pane
    --     (which is visually secondary to the large central master
    --     window) becomes the new dominant master window on the
    --     standard display (this is easy enough to deal with but
    --     is a non-intuitive effect)

    -----------------------------------------------------------------------
    -- Tall-Tabbed Dymamic                                               --
    -----------------------------------------------------------------------
    --
    -- Dynamic 3 pane layout with one tabbed panel using X.L.ComboP
    -- advantage is that it can do a nice 3-up on both ultrawide and
    -- standard (laptop in my case) screen sizes, where the layouts
    -- look like this:
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |----------|       Master       |   Tabs   |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    -- \______________ _______________/\____ _____/
    --                '                     '
    --        this set of panes is      This is a
    --        its' own layout in a      separate
    --        Tall configuration        tab format
    --                                  layout
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |   Tabs   |
    -- |                    |          |
    -- |--------------------|          |
    -- |         |          |          |
    -- ---------------------------------
    -- \_________ _________/\____ _____/
    --           '               '
    -- this set of panes is  This is a
    -- its' own layout in a  separate
    -- Tall configuration    tab format
    --                       layout
    --
    -- Advantages to this use of ComboP to created this dynamic
    -- layout include:
    --
    --   * the center Master stays the same when the layout
    --     changes (unlike the X.L.Master based dyn. layout)
    --
    --   * the Master can have a set of panes under it on the
    --     small screen (standard) layout
    --
    --   * on ultrawide the leftmost pane may be divided into
    --     multiple windows
    --
    --   * possible to toss a tabbed window to the "Master" area
    --     without swapping a window back into tabs
    --
    --   * use of ComboP allows redirection windows to either
    --     left or right section
    --
    -- Disadvantages include:
    --
    --   * normal window swaps fail between the two separate
    --     layouts. There must be a special swap-between-layouts
    --     binding (normal window NAVIGATION works, at least using
    --     X.A.Navigation2D).
    --
    --   * switching between screens can leave title bar clutter
    --     that hasn't been cleaned up properly (restarting
    --     XMonad works to clean this up, but that's hacky)
    --
    --   * somewhat greater need to maintain a mental-map of the
    --     layout (you need to have a sense for the windows being
    --     in separate sections of the different layouts)

    smartTallTabbed = named "Smart Tall-Tabbed"
            $ avoidStruts
            $ ifWider smallMonResWidth wideScreen normalScreen
            where
            wideScreen   = combineTwoP (TwoPane 0.03 (3/4))
                           (smartTall)
                           (smartTabbed)
                           (ClassName "Google-chrome-beta")
            normalScreen = combineTwoP (TwoPane 0.03 (2/3))
                           (smartTall)
                           (smartTabbed)
                           (ClassName "Google-chrome-beta")

    smartTall = named "Smart Tall"
            $ addTopBar
        $ mySpacing
            $ myGaps
        $ boringAuto
            $ ifWider smallMonResWidth wideScreen normalScreen
            where
                wideScreen = reflectHoriz $ Tall 1 0.03 (2/3)
                normalScreen = Mirror $ Tall 1 0.03 (4/5)

    smartTabbed = named "Smart Tabbed"
              $ addTopBar
              $ myCustomGaps
              $ tabbed shrinkText myTabTheme
-}
    -----------------------------------------------------------------------
    -- Flexi Combinators                                                 --
    -----------------------------------------------------------------------
    --
    -- failed attempt. creates a nice looking layout but I'm not sure
    -- how to actually direct tabs to the tabbed area
    --
    --     flexiCombinators = named "Flexi Combinators"
    --             $ avoidStruts
    --             $ ifWider smallMonResWidth wideScreen normalScreen
    --             where
    --             wideScreen   = smartTall ****||* smartTabbed
    --             normalScreen = smartTall ***||** smartTabbed



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


-- myTerminal = "urxvt"
myTerminal     = "/home/frosch03/bin/terminal"
myTmuxTerminal = "/home/frosch03/bin/terminal -e tmux attach-session -t frog"
myFirefox      = "firefox"
myChrome       = "chromium"
myEditor       = "emacsclient -c"
myBrowser      = myFirefox

jiraCommand         = "dex $HOME/.local/share/applications/jira.desktop"
jiraInfix           = "jira"
jiraResource        = "jira.frosch03.de"
isJira              = (resource =? jiraResource)

youtubeCommand      = "dex $HOME/.local/share/applications/youtube.desktop"
youtubeInfix        = "youtube"
youtubeResource     = "www.youtube.com"
isYoutube           = (resource =? youtubeResource)

imCommand      = "dex $HOME/.local/share/applications/signal.desktop"
imInfix        = "im"
imResource     = "signal"
isIm           = (resource =? imResource)

whatsappCommand     = "dex $HOME/.local/share/applications/whatsapp.desktop"
whatsappInfix       = "whatsapp"
whatsappResource    = "web.whatsapp.com"
isWhatsapp          = (resource =? whatsappResource)

orgCaptureCommand     = "dex $HOME/.local/share/applications/org-capture.desktop"
orgCaptureInfix       = "capture"
orgCaptureResource    = "orgCapture"
isOrgCapture          = (resource =? orgCaptureResource)

scratchpads =
    [   (NS "im"       imCommand         isIm         defaultFloating)
    ,   (NS "jira"     jiraCommand       isJira       defaultFloating)
    ,   (NS "youtube"  youtubeCommand    isYoutube    defaultFloating)
    ,   (NS "whatsapp" whatsappCommand   isWhatsapp   defaultFloating)
    ,   (NS "capture"  orgCaptureCommand isOrgCapture defaultFloating)
    ] 

-- My additional keybindings
myKeys x = M.fromList $
  [ ((modMask x,                 xK_p), shellPrompt myXPConfig)
  , ((modMask x .|. shiftMask,   xK_r), spawn "alacritty -e emacsclient -nw -e '(remember)'")
  , ((modMask x .|. shiftMask,   xK_e), spawn myEditor)
  , ((modMask x,                 xK_y), scratchpadSpawnActionTerminal "alacritty")
  , ((modMask x,                 xK_m), spawn "dmpc")
  , ((modMask x .|. shiftMask,   xK_m), manPrompt myXPConfig)
  , ((modMask x .|. shiftMask,   xK_s), sshPrompt myXPConfig)
  , ((modMask x .|. shiftMask,   xK_f), focusUrgent)
  , ((modMask x .|. shiftMask,   xK_j), windows swapDown)
  , ((modMask x .|. shiftMask,   xK_k), windows swapUp)
  , ((modMask x,                 xK_m), windows shiftMaster)
  , ((modMask x .|. shiftMask,   xK_Return), spawn myTerminal)
  , ((modMask x .|. controlMask, xK_l),	     spawn "/home/frosch03/bin/lock")
  , ((0,        xK_XF86Play),      spawn "mpc toggle")
  , ((0,        xK_XF86Stop),      spawn "mpc stop")
  , ((0,        xK_XF86Fwrd),      spawn "mpc next")
  , ((0,        xK_XF86Bwrd),      spawn "mpc prev")
  , ((0,        xK_XF86Thnk),      spawn "/home/frosch03/whatIsThisPlace")
  , ((0,       xK_XF86Sleep),      spawn "sudo pm-suspend")
  , ((0, xK_XF86ScreenSaver),      spawn "gnome-screensaver-command --lock")
  ]
newKeys x = myKeys x `M.union` keys defaultConfig x

-- My additional managed applications (browser is always on desktop 3 and in fullscreen, etc.)
myManagedHook =
        manageSpecific
    <+> manageDocks
    <+> namedScratchpadManageHook scratchpads
    <+> fullscreenManageHook
    <+> manageSpawn
    where manageSpecific = composeAll . concat $
                           [ [ className =? c --> doFloat               | c <- floatClass ]
                           , [ title     =? t --> doFloat               | t <- floatTitle ]
                           , [ title     =? x --> doF (S.shift "hack")  | x <- hacking]
                           , [ title     =? x --> doF (S.shift "conf")  | x <- configuring]
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



------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
--myEventHook = mempty
myEventHook = ewmhDesktopsEventHook



-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=consolas"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

-- any workspace but scratchpad
notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)

nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

wsKeys = map show $ [1..9] ++ [0]
            


-- toggle any workspace but scratchpad
myToggle = windows $ W.view =<< W.tag . head . filter 
           ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden


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
    [ -- ("M-<Space>", addName "Launcher"       $ spawn myLauncher)
      ("M-<Return>"  , addName "Terminal"       $ spawn myTerminal)
    , ("M-S-<Return>", addName "Editor"         $ spawn myEditor)
    , ("M-\\"        , addName "Browser"        $ spawn myBrowser)
    , ("M-n"         , addName "NSP youtube"    $ namedScratchpadAction scratchpads "youtube")
    , ("M-c"         , addName "NSP im"         $ namedScratchpadAction scratchpads "im")
    , ("M-o"         , addName "NSP orgCapture" $ namedScratchpadAction scratchpads "capture")
    -- , ("M1-x"     , addName "NSP Xawtv"      $ namedScratchpadAction scratchpads "xawtv")
    -- , ("M-n"      , addName "NSP Console"    $ namedScratchpadAction scratchpads "console")
    , ("M-s s"       , addName "Cancel submap"  $ return ())
    , ("M-s M-s"     , addName "Cancel submap"  $ return ())
    ] ^++^

    -----------------------------------------------------------------------
    -- Windows
    -----------------------------------------------------------------------

    subKeys "Windows"
    (
    [ ("M-<Backspace>"          , addName "Kill"                            kill1)
    , ("M-S-<Backspace>"        , addName "Kill all"                        $ confirmPrompt hotPromptTheme "kill all" $ killAll)
    , ("M-S-d"                  , addName "Kill other duplicates"           $ killAllOtherCopies)
    , ("M-e"                    , addName "Duplicate w to all ws"           $ windows copyToAll)
    , ("M-S-e"                  , addName "Toggle copy w to all ws"         $ toggleCopyToAll)
    , ("M-d"                    , addName "Browse DuckDuckGo via uzbl"      $ inputPrompt myXPConfig "Internet" ?+ (\x -> spawn ("uzbl-browser 'http://ddg.gg/?q=" ++ x ++ "'")))
    , ("M-u"                    , addName "Hide window to stack"            $ withFocused hideWindow)
    , ("M-S-u"                  , addName "Restore hidden window (FIFO)"    $ popOldestHiddenWindow)

    , ("M-b"                    , addName "Promote"                         $ promote) 

    , ("M-g"                    , addName "Un-merge from sublayout"         $ withFocused (sendMessage . UnMerge))
    , ("M-S-g"                  , addName "Merge all into sublayout"        $ withFocused (sendMessage . MergeAll))

    , ("M-z u"                  , addName "Focus urgent"                    focusUrgent)
    , ("M-z m"                  , addName "Focus master"                    $ windows W.focusMaster)

    , ("M-S-w"                  , addName "Focus up"                        $ windows W.focusUp)
    , ("M-S-s"              	, addName "Focus down"                      $ windows W.focusDown)

    -- , ("M-'"                    , addName "Cycle current tabs D"            $ bindOn LD [("Tabs", windows W.focusDown), ("", onGroup W.focusDown')])
    -- , ("M-;"                    , addName "Cycle current tabs U"            $ bindOn LD [("Tabs", windows W.focusUp), ("", onGroup W.focusUp')])

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
    [ ("M-p"                    , addName "Switch to Project"           $ switchProjectPrompt warmPromptTheme)
    , ("M-S-p"                  , addName "Shift to Project"            $ shiftToProjectPrompt warmPromptTheme)
    , ("M-<Escape>"             , addName "Next non-empty workspace"    $ nextNonEmptyWS)
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
    -- , ("M-c"          , addName "Capture a thought"         $ inputPrompt myXPConfig "Capture" ?+ (\x -> spawn ("/home/frosch03/bin/capture.sh " ++ x)))
    -- , ("M-S-n"        , addName "Emacs remember window"     $ spawn "urxvt -e emacsclient -q -nw -e '(remember)'")
    -- , ("M-S-e"        , addName "Emacs eshell"              $ spawn "urxvt -e emacsclient -q -nw -e '(eshell)'")
    , ("M-t"        , addName "Org Roam capture today"    $ spawn "alacritty -e emacsclient -c -e '(make-capture-today-frame)'")
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
    , ("M-S-q"        , addName "Quit XMonad"               $ confirmPrompt hotPromptTheme "Quit XMonad" $ (spawn "xShutdown") >> io (exitWith ExitSuccess))
    -- , ("M-x"          , addName "Lock screen"               $ spawn "xset s activate")
    -- , ("M-<F4>"       , addName "Print Screen"              $ return ())
  --, ("M-F1"            , addName "Show Keybindings"          $ return ())
    ] ^++^


    -----------------------------------------------------------------------
    -- Resizing
    -----------------------------------------------------------------------

    subKeys "Resize"

    [

    -- following is a hacky hack hack
    --
    -- I want to be able to use the same resize bindings on both BinarySpacePartition and other
    -- less sophisticated layouts. BSP handles resizing in four directions (amazing!) but other
    -- layouts have less refined tastes and we're lucky if they just resize the master on a single
    -- axis.
    --
    -- To this end, I am using X.A.MessageFeedback to test for success on using the BSP resizing
    -- and, if it fails, defaulting to the standard (or the X.L.ResizableTile Mirror variants)
    -- Expand and Shrink commands.
    --
    -- The "sequence_" wrapper is needed because for some reason the windows weren't resizing till
    -- I moved to a different window or refreshed, so I added that here. Shrug.
    
    -- mnemonic: less than / greater than
    --, ("M4-<L>"       , addName "Expand (L on BSP)"     $ sequence_ [(tryMessage_ (ExpandTowards L) (Expand)), refresh])

--      ("C-<L>"                  , addName "Expand (L on BSP)"           $ tryMsgR (ExpandTowards L) (Shrink))
--    , ("C-<R>"                  , addName "Expand (R on BSP)"           $ tryMsgR (ExpandTowards R) (Expand))
--    , ("C-<U>"                  , addName "Expand (U on BSP)"           $ tryMsgR (ExpandTowards U) (MirrorShrink))
--    , ("C-<D>"                  , addName "Expand (D on BSP)"           $ tryMsgR (ExpandTowards D) (MirrorExpand))
--
--    , ("C-S-<L>"                , addName "Shrink (L on BSP)"           $ tryMsgR (ShrinkFrom R) (Shrink))
--    , ("C-S-<R>"                , addName "Shrink (R on BSP)"           $ tryMsgR (ShrinkFrom L) (Expand))
--    , ("C-S-<U>"                , addName "Shrink (U on BSP)"           $ tryMsgR (ShrinkFrom D) (MirrorShrink))
--    , ("C-S-<D>"                , addName "Shrink (D on BSP)"           $ tryMsgR (ShrinkFrom U) (MirrorExpand))

      ("M-["                    , addName "Expand (L on BSP)"           $ tryMsgR (ExpandTowards L) (Shrink))
    , ("M-]"                    , addName "Expand (R on BSP)"           $ tryMsgR (ExpandTowards R) (Expand))
    , ("M-S-["                  , addName "Expand (U on BSP)"           $ tryMsgR (ExpandTowards U) (MirrorShrink))
    , ("M-S-]"                  , addName "Expand (D on BSP)"           $ tryMsgR (ExpandTowards D) (MirrorExpand))

    , ("M-C-["                  , addName "Shrink (L on BSP)"           $ tryMsgR (ShrinkFrom R) (Shrink))
    , ("M-C-]"                  , addName "Shrink (R on BSP)"           $ tryMsgR (ShrinkFrom L) (Expand))
    , ("M-C-S-["                , addName "Shrink (U on BSP)"           $ tryMsgR (ShrinkFrom D) (MirrorShrink))
    , ("M-C-S-]"                , addName "Shrink (D on BSP)"           $ tryMsgR (ShrinkFrom U) (MirrorExpand))

  --, ("M-r"                    , addName "Mirror (BSP rotate)"         $ tryMsgR (Rotate) (XMonad.Layout.MultiToggle.Toggle MIRROR))
  --, ("M-S-C-m"                , addName "Mirror (always)"             $ sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
  --, ("M4-r"                   , addName "BSP Rotate"                  $ sendMessage Rotate)

-- TODO: the following are potentially useful but I won't know till I work with BSP further
--    , ("M4-s"                   , addName "BSP Swap"                    $ sendMessage XMonad.Layout.BinarySpacePartition.Swap)
--    , ("M4-p"                   , addName "BSP Focus Parent"            $ sendMessage FocusParent)
--    , ("M4-n"                   , addName "BSP Select Node"             $ sendMessage SelectNode)
    --, ("M4-m"                   , addName "BSP Move Node"               $ sendMessage MoveNode)

    -- sublayout specific (unused)
    --  ("M4-C-S-."               , addName "toSubl Shrink"               $ toSubl Shrink)
    --, ("M4-C-S-,"               , addName "toSubl Expand"               $ toSubl Expand)
    ]               

              where
		toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
							         [] -> windows copyToAll
				                                 _  -> killAllOtherCopies


focusColor   = myGray -- base03
unfocusColor = myGray -- base00
active       = green  -- base03
inactive     = myGray -- base00
activeWarn   = red

base03  = "#898989"
base02  = "#CCCCCC"
base01  = "#111111"
base00  = "#000000"
-- base03  = "#555555"
-- base02  = "#333333"
-- base01  = "#111111"
-- base00  = "#000000"
hisBase03  = "#002b36"
hisBase02  = "#073642"
hisBase01  = "#586e75"
hisBase00  = "#657b83"
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
-- green   = "#859900"
green   = "#00cc00"

frogs_blue = "#003366"
frogs_blue_dark = "#0a1f34"

frogs_grey = "#d3d3d3"                  

gap    = 12
topbar = 12


myFontSmall = "xft:DejaVuSansMono:antialias=true:pixelsize=10:autohint=true"
myFont      = "xft:DejaVuSansMono:antialias=true:pixelsize=14:autohint=true"
myBigFont   = "xft:DejaVuSansMono:antialias=true:pixelsize=20:autohint=true"
myWideFont  = "xft:DejaVuSansMono:antialias=true:pixelsize=180:autohint=true"
-- myFont      = "-*-terminus-medium-*-*-*-*-160-*-*-*-*-*-*"
-- myBigFont   = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"
-- myWideFont  = "xft:Eurostar Black Extended:"
--             ++ "style=Regular:pixelsize=180:hinting=true"
myGray = "#333333"


topBarTheme = def
    { fontName              = myFontSmall

    , activeColor           = frogs_blue -- green -- "white"
    , activeBorderColor     = frogs_blue

    , inactiveColor         = frogs_blue_dark -- "black"
    , inactiveBorderColor   = frogs_blue_dark -- "black"

    , activeTextColor       = frogs_grey -- myGray -- "white"
    , inactiveTextColor     = frogs_grey -- "black" -- "white"

    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myTabTheme = def
    { fontName              = myFont
    , activeBorderColor     = "black"
    , inactiveBorderColor   = "black"
    , activeColor           = myGray
    , inactiveColor         = "black"
    , activeTextColor       = "white"
    , inactiveTextColor     = "white"
    }

warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Top
    }

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

myShowWNameTheme = def
    { swn_font              = myWideFont
    , swn_fade              = 0.5
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
    }
