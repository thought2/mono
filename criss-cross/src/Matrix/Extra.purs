module Matrix.Extra
  ( module Matrix
  , repeat
  , set
  , get
  , size
  , toArrays
  , toIndexedArray
  ) where

import Prelude
import Common (Size, Position)
import Data.Array as Array
import Data.Maybe (Maybe, maybe)
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Matrix (Matrix, prettyPrintMatrix)
import Matrix as MatrixNative

repeat :: forall a. Size Int -> a -> Matrix a
repeat size' x = MatrixNative.repeat (size' !! d0) (size' !! d1) x

set :: forall a. Position Int -> a -> Matrix a -> Maybe (Matrix a)
set pos x = MatrixNative.set (pos !! d0) (pos !! d1) x

get :: forall a. Position Int -> Matrix a -> Maybe a
get pos = MatrixNative.get (pos !! d0) (pos !! d1)

size :: forall a. Matrix a -> Size Int
size mat = vec2 (MatrixNative.width mat) (MatrixNative.height mat)

toArrays :: forall a. Matrix a -> Array (Array a)
toArrays mat = do
  y <- Array.range 0 (MatrixNative.height mat - 1)
  let
    xs = Array.range 0 (MatrixNative.width mat - 1)
  pure (xs >>= (\x -> maybe [] pure $ MatrixNative.get x y mat))

toIndexedArray :: forall a. Matrix a -> Array { position :: Position Int, value :: a }
toIndexedArray mat =
  MatrixNative.toIndexedArray mat
    # map (\{ x, y, value } -> { position: vec2 x y, value })
