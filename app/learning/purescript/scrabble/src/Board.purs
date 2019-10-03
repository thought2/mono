module Board (init, setWord, ErrSetWord(..)) where

import Prelude
import Common (Direction, Position, Size)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable (class Foldable, foldM)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Matrix.Extra (Matrix)
import Matrix.Extra as Matrix

type Board a
  = Matrix (Field a)

type Field a
  = Maybe a

-- INIT
init :: forall a. Size Int -> Board a
init size = Matrix.repeat size Nothing

-- SET STONE
data ErrSetStone
  = ErrSetStoneOutside
  | ErrSetStoneTakenSame
  | ErrSetStoneTakenDiff

setStone :: forall a. Eq a => Position Int -> a -> Board a -> Either ErrSetStone (Board a)
setStone pos x mat = case getStone pos mat of
  Left ErrGetStoneOutside -> Left ErrSetStoneOutside
  Left ErrGetStoneInsideEmpty -> Either.note ErrSetStoneOutside (Matrix.set pos (Just x) mat)
  Right stone
    | stone == x -> Left ErrSetStoneTakenSame
  Right stone -> Left ErrSetStoneTakenDiff

-- GET STONE
data ErrGetStone
  = ErrGetStoneOutside
  | ErrGetStoneInsideEmpty

getStone :: forall a. Position Int -> Board a -> Either ErrGetStone a
getStone pos board = case Matrix.get pos board of
  Nothing -> Left ErrGetStoneOutside
  Just Nothing -> Left ErrGetStoneInsideEmpty
  Just (Just stone) -> Right stone

-- SET WORD
data ErrSetWord
  = ErrSetWordOutside
  | ErrSetWordMisfit

setWord ::
  forall f a.
  FunctorWithIndex Int f =>
  Eq a =>
  Foldable f =>
  Position Int -> Direction Int -> f a -> Board a -> Either ErrSetWord (Board a)
setWord startPos dir startWord startBoard = foldM trySetStone startBoard indexedWord
  where
  indexedWord = mapWithIndex (/\) startWord

  getPos :: Int -> Position Int
  getPos index = startPos + (dir * pure index)

  trySetStone :: (Board a) -> (Int /\ a) -> Either ErrSetWord (Board a)
  trySetStone board (index /\ x) = case setStone (getPos index) x board of
    Left ErrSetStoneOutside -> Left ErrSetWordOutside
    Left ErrSetStoneTakenSame -> Right board
    Left ErrSetStoneTakenDiff -> Left ErrSetWordMisfit
    Right newBoard -> Right newBoard
