module Main where

import           XMonad

main :: IO ()
main =
  xmonad $
  def
    { borderWidth = 2
    , terminal = "urxvt"
    , normalBorderColor = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    }
