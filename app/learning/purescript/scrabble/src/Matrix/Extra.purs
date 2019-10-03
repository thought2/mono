module Matrix.Extra
  ( module Matrix
  , repeat
  , set
  , get
  ) where

import Common (Size, Position)
import Data.Maybe (Maybe)
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Matrix (Matrix)
import Matrix as MatrixNative

repeat :: forall a. Size Int -> a -> Matrix a
repeat size x = MatrixNative.repeat (size !! d0) (size !! d1) x

set :: forall a. Position Int -> a -> Matrix a -> Maybe (Matrix a)
set pos x = MatrixNative.set (pos !! d0) (pos !! d1) x

get :: forall a. Position Int -> Matrix a -> Maybe a
get pos = MatrixNative.get (pos !! d0) (pos !! d1)
