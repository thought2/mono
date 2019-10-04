module Common where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Typelevel.Num (D2)
import Data.Vec (Vec)
import Prelude (class Show)

type Vec2 a
  = Vec D2 a

type Size a
  = Vec2 a

type Position a
  = Vec2 Int

type Direction a
  = Vec2 a

data Step
  = LeftRight
  | TopDown

type CrossWord
  = { position :: Position Int, step :: Step, word :: String }

-- INSTANCE
derive instance genericStep :: Generic Step _

instance showStep :: Show Step where
  show = genericShow
