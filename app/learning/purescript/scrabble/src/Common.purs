module Common where

import Data.Typelevel.Num (D2)
import Data.Vec (Vec)

type Vec2 a
  = Vec D2 a

type Size a
  = Vec2 a

type Position a
  = Vec2 Int

type Direction a
  = Vec2 a
