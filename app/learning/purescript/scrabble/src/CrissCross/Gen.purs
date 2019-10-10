module CrissCross.Gen (addRandomWord, tryTimes) where

import Prelude
import Control.Monad.Except (ExceptT, except, lift, throwError)
import CrissCross (CrissCross)
import Test.QuickCheck.Gen as Gen
import Common (Step(..))
import Control.Monad.Error.Class (class MonadError, try)
import CrissCross as CrissCross
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe)

-- ADD RANDOM WORD
data ErrAddRandomWord
  = ErrAddRandomWordNotPossible

addRandomWord :: CrissCross -> ExceptT ErrAddRandomWord Gen.Gen CrissCross
addRandomWord crissCross = do
  positions <- lift $ Gen.shuffle $ _.position <$> CrissCross.getFields crissCross
  words <- lift $ Gen.shuffle $ CrissCross.getDict crissCross
  steps <- lift $ Gen.shuffle [ LeftRight, TopDown ]
  arrayFindMap3
    words
    positions
    steps
    ( \word position step ->
        CrissCross.setWord { word, position, step } crissCross
          # Either.hush
    )
    # Either.note ErrAddRandomWordNotPossible
    # except

-- ARRAY FIND 3
arrayFindMap3 ::
  forall a b c d.
  Array a -> Array b -> Array c -> (a -> b -> c -> Maybe d) -> Maybe d
arrayFindMap3 xs1 xs2 xs3 f =
  Array.findMap
    ( \x1 ->
        Array.findMap
          ( \x2 ->
              Array.findMap
                ( \x3 ->
                    f x1 x2 x3
                )
                xs3
          )
          xs2
    )
    xs1

-- TRY TIMES
tryTimes ::
  forall a e m.
  MonadError e m =>
  (a -> m a) -> Int -> a -> m a
tryTimes f 0 x = pure x

tryTimes f i x =
  try (f x)
    >>= case _ of
        Left err -> throwError err
        Right ok -> tryTimes f (i - 1) ok
