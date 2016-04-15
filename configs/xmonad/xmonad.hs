{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}
--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import System.IO 
import Data.List
import Data.Maybe
import XMonad hiding ( (|||) )
import System.Exit
-- import System.IO (hPutStrLn)
-- import XMonad.Util.Dzen
import XMonad.Util.Run(spawnPipe) 
import System.IO (hPutStrLn)
import qualified System.IO.UTF8
import Codec.Binary.UTF8.String

--Layouts:

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat

import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Reflect



 -- Layout tests
import XMonad.Layout.WindowArranger
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.TwoPane
import XMonad.Layout.MouseResizableTile

import XMonad.Layout.MagicFocus


import XMonad.Layout.Spacing



--Prompt:
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Theme
import XMonad.Prompt.Window                                                                                            
import XMonad.Prompt.XMonad

--Util:
import XMonad.Util.Run
import XMonad.Util.Themes
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare   

import XMonad.Util.WindowProperties
import XMonad.Util.NamedScratchpad
-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.Warp
import XMonad.Actions.FocusNth
import XMonad.Actions.Submap
import XMonad.Actions.MouseResize
import XMonad.Actions.NoBorders

--Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook  
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName


 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Char
import Foreign.C.Types(CLong)
import Control.Monad
import Data.Ratio

import Graphics.X11.Xlib.Extras
import Graphics.X11.ExtraTypes.XF86
 
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "xterm"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 2 
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- myWorkspaces    = ["α","β","γ","δ"] ++ map show [ 4..6 ]
-- myWorkspaces    = ["1","2","3","4"]

myWorkspaces =  [ "a:chat", "e:term", "l:mail", "w:webs", "m:mix", "v:virt" ]

--
--			THEME SETTINGS
--


-- Border colors for unfocused and focused windows, respectively.


myNormalBorderColor  = "#AEB2C1"
myFocusedBorderColor = "#AEB2C1"


--fontNamed = "'-xos4-terminus-bold-r-normal-*-14-*-*-*-c-*-iso10646-1'"
darkXPC :: XPConfig
darkXPC = defaultXPConfig
      { font = "xft:Terminus:pixelsize=18:bold" 
      , height   = 20
      , fgColor  = "black"
      , bgColor  = "#AEB2C1"
     -- , bgHLight = "#785"
     -- , fgHLight = "black"
      , promptBorderWidth = 1
      }

myTabConfig = defaultTheme {
	fontName = "xft:Terminus:pixelsize=18:bold"
	,inactiveBorderColor = "#AEB2C1"
	,activeBorderColor = "#AEB2C1"
	,urgentBorderColor = "#AEB2C1"
	,inactiveTextColor = "black"
	,activeTextColor   = "black"
	,urgentTextColor   = "black"
	,inactiveColor	   = "#AEB2C1"
	,activeColor	   = "#B14D7A"
	,urgentColor	   = "white"	
}

--switchSpace :: WorkspaceId -> Int -> WorkspaceId

scratchpads = [ NS "top" "xterm -e top" (title =? "top") defaultFloating ]

-- Default offset of drawable screen boundaries from each physical
-- screen. Anything non-zero here will leave a gap of that many pixels
-- on the given edge, on the that screen. A useful gap at top of screen
-- for a menu bar (e.g. 15)
--
-- An example, to set a top gap on monitor 1, and a gap on the bottom of
-- monitor 2, you'd use a list of geometries like so:
--
-- > defaultGaps = [(18,0,0,0),(0,18,0,0)] -- 2 gaps on 2 monitors
--
-- Fields are: top, bottom, left, right.
--
myDefaultGaps   = [(0,0,0,0)]

 
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
      ,((0, xK_F1), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
	, ((0, xK_F3), shellPrompt       darkXPC ) 
 
 	, ((0, xK_F12), spawn "/usr/bin/xine")
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((controlMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((controlMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
  --  , ((controlMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((controlMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((controlMask,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    -- , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    -- , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    -- , ((modMask,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    --, ((modMask,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((controlMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    -- , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
	
 
    -- Deincrement the number of windows in the master area
    -- , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- toggle the status bar gap
--    , ((modMask              , xK_b     ),
--          modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i
--                             in if n == x then (0,0,0,0) else x))
 
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
	, ((modMask, xK_o), spawn "/home/diesel/bin/touchpadd_toggle.sh")
 
    -- Restart xmonad
   -- , ((modMask, xK_q     ), broadcastMessage ReleaseResources >> restart "xmonad" True)
     , ((modMask, xK_q		), spawn "xmonad --recompile; xmonad --restart")

	
--	, ((modMask, xK_Right), sendMessage $ Go R)   -- Go to client on the right
--	, ((modMask, xK_Left), sendMessage $ Go L)   -- Go to client on the left
--	, ((modMask, xK_Up), sendMessage $ Go U)   -- Go to client above
--	, ((modMask .|. shiftMask, xK_p), sendMessage $ Go D)   -- Go to client above
--	, ((modMask, xK_p), sendMessage $ Go U)   -- Go to client above
--	, ((modMask, xK_Down), sendMessage $ Go D)   -- Go to client below

	]
	++
	[
	-- -------- window transfers --------------- --
	  ((modMask .|. shiftMask, xK_Right), sendMessage $ Swap R)   -- Swap current client with the one on the right
	, ((modMask .|. shiftMask, xK_Left), sendMessage $ Swap L)   -- Swap current client with the one on the left
	, ((modMask .|. shiftMask, xK_Up), sendMessage $ Swap U)   -- Swap current client with the one above
	, ((modMask .|. shiftMask, xK_Down), sendMessage $ Swap D)   -- Swap current client with the one below
	, ((modMask .|. controlMask, xK_Right), sendMessage $ Move R)   -- Move client to the sub layout on the right
	, ((modMask .|. controlMask, xK_Left), sendMessage $ Move L)   -- Move client to the sub layout on the left
	, ((modMask .|. controlMask, xK_Up), sendMessage $ Move U)    -- Move client to the sub layout above
	, ((modMask .|. controlMask , xK_Down), sendMessage $ Move D)   -- Move client to the sub layout below
	-- ------------------------------------------ --
    ]
    ++
	-- ------ Apple-like workspaces switch ----- --
	[ ((controlMask, xK_Right),  switchFrom  	1)
	,((controlMask, xK_Left),	 switchFrom 	(-1))
	,((controlMask, xK_Up), 	 switchFrom 	(-rows))
	,((controlMask, xK_Down), 	 switchFrom  	rows)
	-- ----------------------------------------- --
	]
	++
	-- ------ Alt+Fn / Alt + Shift + Fn workspace switch/desktop transfers ------ --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
	-- -------------------------------------------------------------------------- --	

	++ 
	-- ----- Alt+k n Windows switch ----------- --
	[
		((modMask , xK_k), submap . M.fromList $
		[((0, k), focusNth i) | (i, k) <- zip [0 .. 8] [xK_1 ..]])
	-- ---------------------------------------- --
	]
	++
	-- ----- Switch and move window to particular workspace ----- --
	[
		 ((modMask , xK_a), windows( W.greedyView "a:chat"))
		,((modMask , xK_e), windows( W.greedyView "e:term"))
		,((modMask , xK_l), windows( W.greedyView "l:mail"))
		,((modMask , xK_w), windows( W.greedyView "w:webs"))
		,((modMask , xK_m), windows( W.greedyView "m:mix" ))
		,((modMask , xK_v), windows( W.greedyView "v:virt" ))

		,((modMask .|. shiftMask, xK_a), windows( W.shift "a:chat"))
		,((modMask .|. shiftMask, xK_e), windows( W.shift "e:term"))
		,((modMask .|. shiftMask, xK_l), windows( W.shift "l:mail"))
		,((modMask .|. shiftMask, xK_w), windows( W.shift "w:webs"))
		,((modMask .|. shiftMask, xK_m), windows( W.shift "m:mix" ))
		,((modMask .|. shiftMask, xK_v), windows( W.shift "v:virt" ))
		,((superl_l_mask, xK_m), windows( W.shift "m:mix"))
	]

superl_l_mask::KeyMask
superl_l_mask = 0xffeb

--	++
--	[
--		((modMask .|. controlMask, xK_t), namedScratchpadAction scratchpads "top")
--	]	

switchFrom x = withWindowSet $ \s -> do windows $ W.view (switchSpace (currentTag s)  x)
currentTag s = W.tag . W.workspace . W.current $ s
rows = 2
cols = 2
myNextWorkspace :: Int->Int->Int
myNextWorkspace current turn | current + turn > 0 && current + turn <= rows * cols = current + turn
							 | current + turn == 0 && turn == -1 = rows*cols
							 | current + turn == rows+cols + 1 && turn == 1 = 1
							 | otherwise = current
myElemIndex :: Eq a => a -> [a] -> Int
myElemIndex x a = head (elemIndices x a)
myNextIndex w x = myNextWorkspace ( (myElemIndex w myWorkspaces)+1 ) x
switchSpace w x = myWorkspaces!!((myNextIndex w x)-1)

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout =  windowArrange $ avoidStruts $ fixFocus $ 
	onWorkspace "a:chat" myIM $
	onWorkspace "e:term" ( mytabs |||  magicFocus( centerMaster mytabs ) ||| TwoPane (3/100) (1/2) ||| Grid) $
	onWorkspace "l:mail" mytabs $
	onWorkspace "m:mix" (
		mouseResizableTile |||
		(windowNavigation $ (mytabs ****|* mytabs)) ||| 
		(windowNavigation $ ((mytabs */* mytabs) ****|* mytabs)) |||
		( windowNavigation $ ((mytabs ***/** mytabs) ****|* mytabs)) |||
		Accordion |||
		Circle |||
		Grid |||
		mytabs |||
		TwoPane (3/100) (1/2) |||
		noBorders Full   
	) $
	onWorkspace "w:webs" ( mytabs ||| Accordion ||| TwoPane (3/100) (1/2)  ||| Circle ||| noBorders Full ) $
	onWorkspace "v:virt" mouseResizableTile $
	noBorders Full   
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     --mytabs  = tabbed shrinkText (theme smallClean)
     mytabs  = tabbed shrinkText myTabConfig
	 -- myIM
     -- myIM  =  withIMs ratio rosters chatLayout where
     myIM  =  reflectHoriz $ withIMs ratio rosters chatLayout where
		chatLayout = mytabs
		ratio	   = 1%5
		rosters	   = [pidginRoster, skypeRoster, tomBoy]
		pidginRoster = (ClassName "Pidgin") `And` (Title "Buddy List")
		skypeRoster = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm")) `And` (Not (Role "ConversationsWindow"))
		tomBoy	= (ClassName "Tomboy")

		--pidginRoster = (ClassName "Pidgin") `And` (Title "Buddy List")
     myIMcreazy  =  reflectHoriz $ withIMs ratio rosters chatLayout where
		chatLayout = (windowNavigation $ (mytabs ***/** mytabs))
		ratio	   = 1%5
		rosters	   = [pidginRoster]
		pidginRoster = (ClassName "Pidgin") `And` (Title "Buddy List")

------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageDocks <+>manageMenus<+>manageDialogs<+>composeAll
    [ className =? "MPlayer"        	--> doFloat
    , className =? "Gimp"           	--> doFloat
    , resource  =? "desktop_window" 	--> doIgnore
    , resource  =? "kdesktop"      	    --> doIgnore 
	, className =? "Pidgin"				--> doF(W.shift "a:chat")
	, className =? "Skype"				--> doF(W.shift "a:chat")
	, className =? "Iceweasel"	    	--> doF(W.shift "w:webs")
	, className =? "Chromium-browser"   --> doF(W.shift "w:webs")
	, className =? "Icedove"			--> doF(W.shift "l:mail")
	, className =? "Gnome-terminal"		--> doF(W.shift "e:term")
	, className =? "Mail"				--> doF(W.shift "l:mail")
	, className =? "Thunderbird"		--> doF(W.shift "l:mail")
--	, className =? "xpdf"			--> doF(W.shift "2")
--	, className =? "djvu"			--> doF(W.shift "2")
--	, className =? "chmviewer"		--> doF(W.shift "2")
--	, className =? "fbreader"		--> doF(W.shift "2")
--	, className =? "Do"				--> doFloat
--	, className =? "xine"			--> doFloat]
--, className =? "Firefox-bin"	--> doF(W.shift "4")
--, className =? "Iceweasel"		--> doF(W.shift "4")
--, className =? "xterm"			--> doF(W.shift "2")
--, className =? "XTerm"			--> doF(W.shift "2")
	, className =? "xine"			--> (ask >>= \w -> liftX (toggleBorder w) >> doCenterFloat)
	, className =? "Vlc"			--> (ask >>= \w -> liftX (toggleBorder w) >> doCenterFloat)]
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
--     -myLogHook = return ()
 
------------------------------------------------------------------------




myLogHook h =  dynamicLogWithPP $ xmobarPP {
                ppCurrent  = xmobarColor "black" "#B14D7A" . pad 
              , ppVisible  = xmobarColor "black" "#AEB2C1" . pad
              , ppHidden   = xmobarColor "black" "#AEB2C1" . pad
              , ppHiddenNoWindows = xmobarColor "black" "#AEB2C1" . pad
              , ppUrgent   = xmobarColor "yellow" "black"
              , ppWsSep    = "|"
              , ppSep      = " - "
              , ppLayout   = id
              , ppTitle    = ("^fg(white)" ++) 
			  , ppOrder  = \(ws:_) -> [ws]
              , ppOutput   = System.IO.UTF8.hPutStrLn h
              }


-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
-- main =  dzen $ \conf ->  xmonad $ defaults
--myStatusBar = "dzen2 -x '0' -y '0' -h '16' -w '1320' -ta 'l' -fg '#f0f0f0' -bg '#0f0f0f' -fn '-xos4-terminus-medium-r-normal-*-14-*-*-*-c-*-iso10646-1'"
--myTopBar = "/home/diesel/bin/bin/conky .conkyrc/conky-bar| dzen2" -- -x '1320' -y '0' -h '16' -w '600' -ta 'r' -fg '#555555' -bg '#0f0f0f' -fn '-xos4-terminus-medium-r-normal-*-14-*-*-*-c-*-iso10646-1'"
--myBottomBar = "conky -c .conkybottomrc | dzen2 -x '0' -y '1184' -h '16' -w '1920' -ta 'l' -fg '#555555' -bg '#0f0f0f' -fn '-xos4-terminus-medium-r-normal-*-14-*-*-*-c-*-iso10646-1'"

--firstDzenCommand = "dzen2 -fn " ++ fontNamed ++ " -bg '#AEB2C1' -fg 'black' -h 16 -w 139 -sa c -e '' -ta l -x '0' -y '0'"
--secondDzenCommand = "/home/diesel/bin/conky -c /home/diesel/.conkyrc/conky-bar -u 1 | dzen2 -fn " ++ fontNamed ++ " -bg '#AEB2C1' -fg 'black' -h '16' -w '780' -sa c -e '' -ta r -x '140' -y '0'"

-- main =  do 
--	din <- spawnPipe firstDzenCommand
--    spawnPipe secondDzenCommand
--	din <- spawnPipe myTopBar
--	dzen <- spawnPipe myStatusBar
--	dzenbottom <- spawnPipe myBottomBar
--	dynHooksRef <- initDynamicHooks

main = do  
	din <- spawnPipe "/usr/bin/xmobar /home/diesel/.xmobarrc"
          -- spawnPipe secondDzenCommand
        xmonad  $ withUrgencyHook NoUrgencyHook $ defaultConfig {

        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        --numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
		handleEventHook = ewmhDesktopsEventHook,
		startupHook = ewmhDesktopsStartup >> setWMName "LG3D",

        --defaultGaps        = myDefaultGaps,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
--		logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn din}
    	logHook            = do
								ewmhDesktopsLogHook
								myLogHook din
--        logHook            = ewmhDesktopsLogHook

}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- That's for dialog windows Floating				  --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Window Property
getProp :: Atom -> Window -> X (Maybe [CLong])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w
-- Checks if window propery set to the value 
checkAtom name value = ask >>= \w -> liftX $ do
          a <- getAtom name
          val <- getAtom value
          mbr <- getProp a w
          case mbr of
            Just [r] -> return $ elem (fromIntegral r) [val]
            _ -> return False

-- Cheks if the window is Dialog
checkDialog = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
-- Checks if the window is tear-off menu:
checkMenu = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_MENU"

manageMenus = checkMenu --> doCenterFloat
manageDialogs = checkDialog --> doCenterFloat

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- IM Layouts exprience
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)

instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"


-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props

-- | IM layout modifier applied to the Grid layout
gridIMs :: Rational -> [Property] -> ModifiedLayout AddRosters Grid a
gridIMs ratio props = withIMs ratio props Grid

hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w

-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> W.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))

applyIMs ratio props wksp rect = do
    let stack = W.stack wksp
	-- Flatten a possibly emtpy stack into a list
    let ws = W.integrate' $ stack
	-- get the list of "rosters" from stackset
    rosters <- filterM (hasAnyProperty props) ws
	-- get the rosters list length?
    let n = fromIntegral $ length rosters
    -- hmm? split Horizontally what? anyways we get rosters rectangle and chat's rectangle
    let (rostersRect,chatsRect) = if n == 0 then
									splitHorizontallyBy 0 rect
								  else
									splitHorizontallyBy (ratio) rect

    --let (rostersRect,chatsRect) = splitHorizontallyBy (n * ratio) rect
	-- split Rosters rectangle on n parts.
    let rosterRects = splitVertically n rostersRect
	-- returns the elements of rosters such that `notElem` - true. WTF `notElem`? into filteredStack
    let filteredStack = stack >>= W.filter (`notElem` rosters)
	-- here we apply the settings to workspace?
    wrs <- runLayout (wksp {W.stack = filteredStack}) chatsRect
    return ((zip rosters rosterRects) ++ fst wrs, snd wrs)


data FixFocus a = FixFocus (Maybe a) deriving (Read, Show)
instance LayoutModifier FixFocus Window where
    modifyLayout (FixFocus mlf) ws@(W.Workspace id lay Nothing) r = runLayout ws r
    modifyLayout (FixFocus Nothing) ws r = runLayout ws r
    modifyLayout (FixFocus (Just lf)) (W.Workspace id lay (Just st)) r = do
        let stack_f = W.focus st  -- get current stack's focus
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let mreal_f = maybe Nothing (Just . W.focus) mst -- get Maybe current real focus
        is_rf_floating <- maybe (return False) (\rf -> withWindowSet $ return . M.member rf . W.floating) mreal_f -- real focused window is floating?
        let new_stack_f = if is_rf_floating then lf else stack_f --if yes: replace stack's focus with our last saved focus
        let new_st' = until (\s -> new_stack_f == W.focus s) W.focusUp' st -- new stack with focused new_stack_f
        let new_st = if (new_stack_f `elem` (W.integrate st)) then new_st' else st -- use it only when it's possible to
        runLayout (W.Workspace id lay (Just new_st)) r

    redoLayout (FixFocus mlf) r Nothing wrs = return (wrs, Just $ FixFocus mlf)
    redoLayout (FixFocus mlf) r (Just st) wrs = do
        let stack_f = W.focus st  -- get current stack's focus
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let mreal_f = maybe Nothing (Just . W.focus) mst -- get Maybe current real focus
        let crf_in_stack = maybe False ((flip elem) (W.integrate st)) mreal_f -- current real focus belongs to stack?
        let new_saved_f = if crf_in_stack then fromJust mreal_f else stack_f -- if yes: replace saved focus
        return (wrs, Just $ FixFocus $ Just new_saved_f)

fixFocus = ModifiedLayout $ FixFocus Nothing

--------------
--exprimetns
--------------

data VSimplest a = VSimplest deriving (Show, Read)
instance LayoutClass VSimplest a where
	pureLayout VSimplest rec (W.Stack w l r) = zip (w : reverse l ++ r) (repeat rec)
	
