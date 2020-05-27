-------------
-- Modules --
-------------

import XMonad
import Data.Monoid
import Data.Maybe
import System.Exit
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import System.IO
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks 
import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.Minimize
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Minimize
import XMonad.Layout.BoringWindows (focusUp, focusDown, focusMaster, boringWindows)
import XMonad.Layout.MultiColumns
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-----------------
-- Definitions --
-----------------


myTerminal                = "termite"
myFileManager             = "pcmanfm"
myPulse                   = "pavucontrol"
myCalculator              = "galculator"
myFocusFollowsMouse       = False
myClickJustFocuses        = False
myBorderWidth             = 2
myModMask                 = mod4Mask
myWorkspaces              = map show [1..9] ++ ["NSP"] 
myNormalBorderColor       = "#000000"
myFocusedBorderColor      = "#00ff00"
myWindows                 = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------
-- Key bindings --
------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm                                , xK_Return          ), spawn $ XMonad.terminal conf) -- launch terminal
    , ((modm .|. controlMask                , xK_equal           ), spawn "dmenu_run -b -fn 'Source Code Pro-9' -nb '#000000' -nf '#00ff00' -sb '#00ff00' -sf '#000000'") -- launch dmenu
    , ((modm                                , xK_equal           ), spawn "rofi -show drun -theme /home/kriket/.config/rofi/grid.rasi") -- launch rofi
    , ((modm .|. controlMask                , xK_BackSpace       ), kill) -- kill window
    , ((modm                                , xK_grave           ), kill)
    , ((modm                                , xK_space           ), sendMessage NextLayout) -- cycle layouts
    , ((modm .|. shiftMask                  , xK_space           ), setLayout $ XMonad.layoutHook conf) -- reset layouts
    , ((modm                                , xK_o               ), refresh) -- snap windows to default size
    , ((modm                                , xK_Right           ), focusDown) -- next window 
    , ((modm                                , xK_Left            ), focusUp) -- prev window
    , ((modm                                , xK_l               ), focusMaster) -- focus master
    , ((modm                                , xK_f               ), withFocused float)
    , ((modm .|. shiftMask                  , xK_Return          ), windows W.swapMaster) -- swap to master
    , ((modm .|. controlMask                , xK_Right           ), windows W.swapDown) -- swap next
    , ((modm .|. controlMask                , xK_Left            ), windows W.swapUp) -- swap previous
    , ((modm .|. shiftMask                  , xK_Left            ), sendMessage Shrink) -- shrink master
    , ((modm .|. shiftMask                  , xK_Right           ), sendMessage Expand) -- expand master
    , ((modm                                , xK_t               ), withFocused $ windows . W.sink) -- floating back to tiling
    , ((modm                                , xK_Up              ), moveTo Next nonNSP) -- next workspace
    , ((modm                                , xK_Down            ), moveTo Prev nonNSP) -- previous workspace
    , ((modm                                , xK_comma           ), sendMessage (IncMasterN 1)) -- increase master windows
    , ((modm                                , xK_period          ), sendMessage (IncMasterN (-1))) -- decrease master windows
    , ((modm                                , xK_Delete          ), sendMessage ToggleStruts) -- toggle status bar
    , ((modm                                , xK_m               ), withFocused minimizeWindow) -- minimize window
    , ((modm                                , xK_n               ), withLastMinimized maximizeWindowAndFocus) -- restore minimized
    , ((modm .|. shiftMask                  , xK_q               ), io (exitWith ExitSuccess)) -- quit xmonad
    , ((modm .|. controlMask                , xK_r               ), spawn "xmonad --recompile; xmonad --restart") -- restart xmonad

    -- Scratchpads
    , ((modm                                , xK_End             ), namedScratchpadAction myScratchPads "smallterminal")
    , ((modm                                , xK_Page_Down       ), namedScratchpadAction myScratchPads "bigterminal")
    , ((modm                                , xK_Home            ), namedScratchpadAction myScratchPads "music")
    , ((modm .|. controlMask                , xK_End             ), namedScratchpadAction myScratchPads "xmonadconfig")
    , ((modm                                , xK_Page_Up         ), namedScratchpadAction myScratchPads "filemanager")
    , ((modm .|. controlMask                , xK_Home            ), namedScratchpadAction myScratchPads "weechat")
    , ((modm                                , xK_backslash       ), namedScratchpadAction myScratchPads "pulse")
    , ((modm                                , xK_bracketright    ), namedScratchpadAction myScratchPads "calc") 
    
    -- App launchers
    , ((modm .|. controlMask                , xK_0               ), spawn "firefox-bin")
    , ((modm .|. controlMask                , xK_1               ), spawn "pavucontrol")
    , ((modm .|. controlMask                , xK_2               ), spawn "gcolor2")

    -- cmus controls
    , ((modm .|. shiftMask                  , xK_Home            ), spawn "cmus-remote -u")
    , ((modm .|. shiftMask                  , xK_Page_Up         ), spawn "cmus-remote -n")
    , ((modm .|. shiftMask                  , xK_Page_Down       ), spawn "cmus-remote -r")
    , ((modm .|. shiftMask                  , xK_End             ), spawn "cmus-remote -s") 
    , ((modm .|. shiftMask                  , xK_period          ), spawn "cmus-remote -k +10")
    , ((modm .|. shiftMask                  , xK_comma           ), spawn "cmus-remote -k -10")

    -- suspend/lock
    , ((modm .|. shiftMask .|. controlMask  , xK_BackSpace       ), spawn "sudo pm-suspend && i3lock -i /home/kriket/Pictures/yuk.png -C -c 000000ff")
    , ((modm .|. controlMask                , xK_Delete          ), spawn "i3lock -i /home/kriket/Pictures/yuk.png -t -c 000000")
    ]
    ++
    -- workspaces
    
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    where nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

--------------------
-- Mouse bindings --
--------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm                 , button1), (\w -> focus w >> mouseMoveWindow w
                                                        >> windows W.shiftMaster))
    , ((modm                 , button3), (\w -> focus w >> mouseResizeWindow w
                                                        >> windows W.shiftMaster))
    , ((modm .|. controlMask , button1), (\w -> focus w >> kill))  
    ]

-------------
-- Layouts --
-------------

myGaps      = spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True  

loTiled     = renamed [Replace "Tiled"] (myGaps . minimize $ Tall 1 (1/300) (1/2))
loThree     = renamed [Replace "Three"] (myGaps . minimize $ ThreeColMid 1 (3/100) (1/2))
loGrid      = renamed [Replace "Grid"]  (myGaps . minimize $ Grid)
loCols      = renamed [Replace "Cols"]  (myGaps . minimize $ multiCol [1] 1 0.01 (-0.5))
loFull      = renamed [Replace "Full"]  (minimize $ Full)
loFloat     = renamed [Replace "Float"] (minimize $ simplestFloat)

myLayout0   = loThree ||| loTiled ||| loGrid  ||| loCols ||| loFull ||| loFloat
myLayout1   = loFull  ||| loTiled ||| loThree ||| loGrid ||| loCols ||| loFloat
myLayout2   = loTiled ||| loThree ||| loGrid  ||| loCols ||| loFull ||| loFloat

myWSLayouts = onWorkspace "1" myLayout1 $
              onWorkspace "2" myLayout0 $
              onWorkspace "3" myLayout2 $
              onWorkspace "4" myLayout0 $
              onWorkspace "7" myLayout0 $
              myLayout2

myLayout    = avoidStruts $ smartBorders $ boringWindows $ myWSLayouts

------------------
-- Window rules --
------------------

myManageHook :: ManageHook
myManageHook = composeAll 
    [ className =? "MPlayer"              --> doFloat
    , className =? "Gimp"                 --> doFloat
    , className =? "thunar"               --> doFloat
    , className =? "Viewnior"             --> doFloat
    , className =? "Pavucontrol"          --> doFloat
    , className =? "Gcolor2"              --> doFloat
    , className =? "Pcmanfm"              --> doFloat
    , className =? "Blueman-manager"      --> doFloat
    , className =? "Gucharmap"            --> doFloat
    , className =? "Mousepad"             --> doFloat
    , resource  =? "desktop_window"       --> doIgnore
    , resource  =? "kdesktop"             --> doIgnore 
    , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    ] <+> namedScratchpadManageHook myScratchPads

--------------------
-- Event handling --
--------------------

myEventHook = mempty

-----------------------------
-- Status bars and logging --
-----------------------------

myLogHook = return ()

-------------
-- Startup --
-------------

myStartupHook = do
	spawnOnce "nitrogen --restore &"
	spawnOnce "picom &"

------------
-- XMonad --
------------ 

main = do 
    xmproc <- spawnPipe "xmobar -x 0 /home/kriket/.xmonad/xmobar/xmobar.config"
    xmonad $ docks defaults 
        { manageHook  = manageDocks <+> ( isFullscreen --> doFullFloat ) <+> myManageHook
        , logHook     = let noScratchpad ws = if ws == "NSP" then "" else ws
                            pp = xmobarPP { ppOutput          = hPutStrLn xmproc
                                          , ppCurrent         = xmobarColor "grey" ""  . wrap "<fc=#00ff00>(</fc>" "<fc=#00ff00>)</fc>"
                                          , ppHidden          = xmobarColor "#00aa00" "" . noScratchpad
                                          , ppHiddenNoWindows = xmobarColor "grey" "" . noScratchpad
                                          , ppTitle           = xmobarColor "green"  "" . shorten 65
                                          , ppVisible         = wrap "(" ")"
                                          , ppUrgent          = xmobarColor "red" "yellow"
                                          , ppExtras          = [myWindows]
                                          }
                        in do
                        dynamicLogWithPP pp

        , startupHook = setWMName "XMonad" <+> myStartupHook 
        }`additionalKeys`
            [ ((0                 , 0x1008FF12), spawn "amixer set Master toggle"),
              ((0                 , 0x1008FF11), spawn "amixer -q sset Master 2%-"),
              ((0                 , 0x1008FF13), spawn "amixer -q sset Master 2%+"),
              ((0                 , 0x1008FF02), spawn "lux -a 5%"),
              ((0                 , 0x1008FF03), spawn "lux -s 5%"),
              ((0                 , 0x1008FF14), spawn "cmus-remote -u"),
              ((0                 , xK_Print),   spawn "xfce4-screenshooter")
            ]

-----------------
-- Scratchpads --
-----------------

myScratchPads = [ NS "bigterminal"     spawnBigTerm idBigTerm createBigTerm
                , NS "smallterminal"   spawnSmallTerm idSmallTerm createSmallTerm
                , NS "music"           spawnMusic idMusic createMusic
                , NS "xmonadconfig"    spawnXMonadConfig idXMonadConfig createXMonadConfig
                , NS "filemanager"     spawnFM idFM createFM
                , NS "weechat"         spawnWeechat idWeechat createWeechat
                , NS "pulse"           spawnPulse idPulse createPulse
                , NS "calc"            spawnCalc idCalc createCalc
                ]

    where
    spawnBigTerm       = myTerminal ++  " --name=scratchpad1"
    idBigTerm          = resource =? "scratchpad1"
    createBigTerm      = customFloating $ W.RationalRect l t w h
                         where
                         h = 0.9
                         w = 0.9
                         t = 0.95 -h
                         l = 0.95 -w
    spawnSmallTerm     = myTerminal ++  " --name=scratchpad2"
    idSmallTerm        = resource =? "scratchpad2"
    createSmallTerm    = customFloating $ W.RationalRect l t w h
                         where
                         h = 0.55
                         w = 0.55
                         t = 0.77 -h
                         l = 0.77 -w
    spawnMusic         = myTerminal ++ " --name=MUSIC -e /home/kriket/.local/bin/launchcmus.sh"
    idMusic            = resource =? "MUSIC"
    createMusic        = customFloating $ W.RationalRect l t w h 
                         where
                         h = 0.8 
                         w = 0.8
                         t = 0.9 -h
                         l = 0.9 -w
    spawnXMonadConfig  = myTerminal ++ " --name=xmonadconfig -e /home/kriket/.local/bin/launchxmonadconfig.sh" 
    idXMonadConfig     = resource =? "xmonadconfig"
    createXMonadConfig = customFloating $ W.RationalRect l t w h
                         where
                         h = 0.9
                         w = 0.9
                         t = 0.95 -h
                         l = 0.95 -w
    spawnFM            = myFileManager
    idFM               = resource =? "pcmanfm"
    createFM           = customFloating $ W.RationalRect l t w h 
                         where
                         h = 0.5
                         w = 0.5
                         t = 0.9 -h
                         l = 0.6 -w
    spawnWeechat       = myTerminal ++ " --name=weechat -e weechat"
    idWeechat          = resource =? "weechat"
    createWeechat      = customFloating $ W.RationalRect l t w h
                         where
                         h = 0.96
                         w = 0.96
                         t = 0.98 -h
                         l = 0.98 -w
    spawnPulse         = myPulse
    idPulse            = resource =? "pavucontrol"
    createPulse        = customFloating $ W.RationalRect l t w h 
                         where
                         h = 0.55
                         w = 0.55
                         t = 0.77 -h
                         l = 0.77 -w
    spawnCalc         = myCalculator
    idCalc            = resource =? "galculator"
    createCalc        = customFloating $ W.RationalRect l t w h 
                         where
                         h = 0.25
                         w = 0.25
                         t = 0.40 -h
                         l = 0.80 -w

--------------
-- Defaults --
-------------- 

defaults = def {
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    clickJustFocuses   = myClickJustFocuses,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    layoutHook         = myLayout,
    handleEventHook    = myEventHook,
    logHook            = myLogHook
}

