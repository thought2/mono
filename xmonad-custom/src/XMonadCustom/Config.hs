module XMonadCustom.Config
  ( makeConfig
  ) where

import           XMonad                               hiding ((|||))
import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Actions.FloatKeys
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.FloatNext
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.FixedColumn
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutScreens
import           XMonad.Layout.Maximize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.TwoPane
import           XMonad.StackSet                      (RationalRect (RationalRect))
import           XMonad.Util.EZConfig                 (additionalKeysP)
import           XMonadCustom.EnvConfig

import           XMonad.Hooks.FadeInactive

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.8


modm = mod4Mask

makeConfig envConfig =
  defaultConfig
    { focusedBorderColor = border $ colors envConfig
    , borderWidth = 4
    , modMask = modm
    , layoutHook = layout
    , manageHook = myManageHook
    , startupHook = setWMName "LG3D"
    , logHook = myLogHook
    } `additionalKeysP`
  (shortcuts $ tools envConfig)

shortcuts tools =
  [ ("M-C-f", spawn $ firefox tools)
  , ("M-C-g", toggleFloatNext)
  , ("M-C-b", spawn $ thunderbird tools)
  , ( "M-C-s"
    , spawn $
      sleep tools <> " 0.2; " <> scrot tools <> "  -s -e 'mv $f ~/screenshots/'")
  , ("M-C-c", spawn $ chromium tools)
  , ("M-C-a", spawn $ chromium tools <> " --app='http://ddg.gg'")
  , ( "M-C-d"
    , floatNext True >> (spawn $ chromium tools <> " --app='http://ddg.gg'"))
  , ("M-C-e", spawn $ emacs tools <> " --no-splash")
  , ("M-C-t", spawn $ xterm tools)
  , ("M-C-p", floatNext True >> (spawn $ pavucontrol tools))
  , ("M-r t o", withFocused (\winId -> setOpacity winId 1.0))
  , ("M-r t t", withFocused (\winId -> setOpacity winId 0.5))
  , ( "M-C-<Return>"
    , spawn $ rofi tools <> " -matching regex -separator-style none -show run")
  , ("M-C-f", sendMessage $ JumpToLayout "Full")
  , ( "M-C-n"
    , floatNext True >>
      (spawn $ xterm tools <> " -e " <> nmtui tools))
  , ("M-S-<Space>", virtualScreens)
  , ("M-C-S-<Space>", rescreen)
  , ("M-C-l", spawn $ i3lock tools)
  -- cycle workspaces
  , ("M-<Left>", DO.moveTo Prev HiddenNonEmptyWS)
  , ("M-<Right>", DO.moveTo Next HiddenNonEmptyWS)
  , ("M-y", withFocused (sendMessage . maximizeRestore))
  , ("M-C-<Left>", withFocused (keysMoveWindow (-30, 0)))
  , ("M-C-<Right>", withFocused (keysMoveWindow (30, 0)))
  , ("M-C-<Up>", withFocused (keysMoveWindow (0, -30)))
  , ("M-C-<Down>", withFocused (keysMoveWindow (0, 30)))
  , ("M-C-S-<Left>", withFocused (keysResizeWindow (-30, 0) (0, 0)))
  , ("M-C-S-<Right>", withFocused (keysResizeWindow (30, 0) (0, 0)))
  , ("M-C-S-<Up>", withFocused (keysResizeWindow (0, -30) (0, 0)))
  , ("M-C-S-<Down>", withFocused (keysResizeWindow (0, 30) (0, 0)))
  , ("<XF86AudioMute>", audioMute)
  , ("M-C-m", audioMute)
  , ("<XF86AudioRaiseVolume>", audioUp)
  , ("M-C-S-.", audioUp)
  , ("<XF86AudioLowerVolume>", audioDown)
  , ("M-C-S-,", audioDown)
  , ("M-a", spawn $ xdotool tools <> " click 1")
  , ("M-s", spawn $ xdotool tools <> " click 2")
  , ("M-d", spawn $ xdotool tools <> " click 3")
  , ( "M-C-z"
    , floatNext True >> spawn (showKeyboard tools))
  ]
  where
    audioMute = spawn $ pactl tools <> " set-sink-mute 0 toggle"
    audioUp = spawn $ pactl tools <> " set-sink-volume 0 +10%"
    audioDown = spawn $ pactl tools <> " set-sink-volume 0 -10%"
    virtualScreens = layoutScreens 3 $ spacingWithEdge 10 $ TwoPane 0.55 0.45

layout = maximize (tall ||| tall2 ||| full ||| FixedColumn 1 20 80 10)
  where
    tall = Tall 1 (3 / 100) (1 / 2)
    tall2 = Mirror tall
    full = noBorders Full

myManageHook =
  composeAll
    [ liftX willFloatNext --> doRectFloat (RationalRect 0.5 0.02 0.48 0.48)
    , floatNextHook
    , manageHook defaultConfig
    ]
